{- HLINT ignore "Use head" -}
{- HLINT ignore "Use void" -}
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, catch, evaluate)
import Control.Monad (replicateM_)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Exts.Heap (Box (..), GenClosure (..), asBox, getClosureData)
import Hypermedia.Datastar
import Network.HTTP.Types (queryToQueryText, status200, status400, status404)
import Network.Wai (Application, Request, Response, pathInfo, queryString, requestMethod, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import System.IO.Unsafe (unsafePerformIO)
import System.Mem (performGC)

-- Mode configuration
data Mode = Mode
  { modeName :: Text
  , modeDesc :: Text
  , modeSetup :: IO ()
  , modeRun :: Maybe (IO ())
  }

-- Expression constructors - NOINLINE + () argument prevents GHC from
-- compiling these as CAFs, so each call creates completely fresh thunks.
-- Without this, [1..10] etc. get shared and stay forced after the first run.

mkSimpleExpr :: () -> [Int]
mkSimpleExpr () = [1, 2, 3, 4, 5] ++ map (* 10) [6, 7, 8]
{-# NOINLINE mkSimpleExpr #-}

mkMapExpr :: () -> [Int]
mkMapExpr () = map (* 2) [1 .. 10]
{-# NOINLINE mkMapExpr #-}

mkFibsExpr :: () -> [Int]
mkFibsExpr () = take 15 fibs
 where
  fibs :: [Int]
  fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
{-# NOINLINE mkFibsExpr #-}

-- Available modes

simpleList :: Mode
simpleList =
  Mode
    { modeName = "simple-list"
    , modeDesc = "[1,2,3,4,5] ++ map (*10) [6,7,8]"
    , modeSetup = do
        let expr = mkSimpleExpr ()
        _ <- evaluate expr
        atomically $ do
          writeTVar currentExpression (asBox expr)
          writeTVar currentExprDesc "[1,2,3,4,5] ++ map (*10) [6,7,8]"
    , modeRun = Nothing
    }

liveMap :: Mode
liveMap =
  Mode
    { modeName = "live-map"
    , modeDesc = "map (*2) [1..10]"
    , modeSetup = do
        let expr = mkMapExpr ()
        atomically $ do
          writeTVar currentExpression (asBox expr)
          writeTVar currentExprDesc "map (*2) [1..10]"
    , modeRun = Just $ do
        let expr = mkMapExpr ()
        atomically $ do
          writeTVar currentExpression (asBox expr)
          writeTVar currentExprDesc "map (*2) [1..10]"
        _ <- forkIO $ forceListSlowly expr
        pure ()
    }

liveFibs :: Mode
liveFibs =
  Mode
    { modeName = "live-fibs"
    , modeDesc = "take 15 fibs"
    , modeSetup = do
        let expr = mkFibsExpr ()
        atomically $ do
          writeTVar currentExpression (asBox expr)
          writeTVar currentExprDesc "take 15 fibs"
    , modeRun = Just $ do
        let expr = mkFibsExpr ()
        atomically $ do
          writeTVar currentExpression (asBox expr)
          writeTVar currentExprDesc "take 15 fibs"
        _ <- forkIO $ forceListSlowly expr
        pure ()
    }

allModes :: [(String, Mode)]
allModes =
  [ ("simple-list", simpleList)
  , ("live-map", liveMap)
  , ("live-fibs", liveFibs)
  ]

-- Force a list spine + elements one by one with delay
forceListSlowly :: [Int] -> IO ()
forceListSlowly [] = pure ()
forceListSlowly (x : xs) = do
  _ <- evaluate x
  threadDelay 1000000
  forceListSlowly xs

-- Heap node representation
data HeapNode = HeapNode
  { nodeType :: Text
  , nodeValue :: Maybe Text
  , nodePointers :: [Text]
  }

-- Global state
globalBoxMap :: TVar (Map Text Box)
{-# NOINLINE globalBoxMap #-}
globalBoxMap = unsafePerformIO $ newTVarIO Map.empty

currentExpression :: TVar Box
{-# NOINLINE currentExpression #-}
currentExpression = unsafePerformIO $ newTVarIO $ asBox ()

currentExprDesc :: TVar Text
{-# NOINLINE currentExprDesc #-}
currentExprDesc = unsafePerformIO $ newTVarIO ""

-- Get closure data from a Box (unwrap the Box to see what's inside)
getBoxClosure :: Box -> IO (GenClosure Box)
getBoxClosure (Box a) = getClosureData a

boxAddr :: Box -> Text
boxAddr = T.pack . show

-- Walk the heap from a root Box via DFS
walkHeap :: Box -> Int -> IO (Text, Map Text HeapNode)
walkHeap startBox maxDepth = do
  nodesRef <- newTVarIO Map.empty
  boxMapRef <- newTVarIO Map.empty
  visitedRef <- newTVarIO Set.empty

  walkNode nodesRef boxMapRef visitedRef startBox 0

  nodes <- readTVarIO nodesRef
  boxMap <- readTVarIO boxMapRef
  atomically $ modifyTVar' globalBoxMap (const boxMap)

  pure (boxAddr startBox, nodes)
 where
  walkNode nodesRef boxMapRef visitedRef box depth
    | depth > maxDepth = pure ()
    | otherwise = do
        let addr = boxAddr box
        visited <- readTVarIO visitedRef
        if Set.member addr visited
          then pure ()
          else do
            atomically $ do
              modifyTVar' visitedRef (Set.insert addr)
              modifyTVar' boxMapRef (Map.insert addr box)

            closure <- getBoxClosure box

            -- Follow indirections transparently
            closure' <- case closure of
              IndClosure{indirectee = ptr} -> getBoxClosure ptr
              BlackholeClosure{indirectee = ptr} -> getBoxClosure ptr
              _ -> pure closure

            case closure' of
              ConstrClosure{ptrArgs = ptrs, dataArgs = dargs, name = n, modl = m} -> do
                let fullName = T.pack m <> "." <> T.pack n
                    ptrAddrs = map boxAddr ptrs
                if length ptrs >= 2 && T.pack n == ":"
                  then do
                    -- Cons cell: first ptr is head, second is tail
                    let headBox = ptrs !! 0
                        tailBox = ptrs !! 1
                    headVal <- getHeadValue headBox
                    let node = HeapNode "cons" headVal [boxAddr tailBox]
                    atomically $ modifyTVar' nodesRef (Map.insert addr node)
                    walkNode nodesRef boxMapRef visitedRef headBox (depth + 1)
                    walkNode nodesRef boxMapRef visitedRef tailBox (depth + 1)
                  else
                    if null ptrs && (T.pack n == "[]")
                      then do
                        let node = HeapNode "nil" Nothing []
                        atomically $ modifyTVar' nodesRef (Map.insert addr node)
                      else do
                        let val = case dargs of
                              [] -> Nothing
                              [v] -> Just (T.pack (show v))
                              vs -> Just (T.pack (show vs))
                        let node = HeapNode ("constr:" <> fullName) val ptrAddrs
                        atomically $ modifyTVar' nodesRef (Map.insert addr node)
                        mapM_ (\p -> walkNode nodesRef boxMapRef visitedRef p (depth + 1)) ptrs
              ThunkClosure{ptrArgs = ptrs} -> do
                let ptrAddrs = map boxAddr ptrs
                    node = HeapNode "thunk" Nothing ptrAddrs
                atomically $ modifyTVar' nodesRef (Map.insert addr node)
                mapM_ (\p -> walkNode nodesRef boxMapRef visitedRef p (depth + 1)) ptrs
              FunClosure{ptrArgs = ptrs} -> do
                let ptrAddrs = map boxAddr ptrs
                    node = HeapNode "function" Nothing ptrAddrs
                atomically $ modifyTVar' nodesRef (Map.insert addr node)
                mapM_ (\p -> walkNode nodesRef boxMapRef visitedRef p (depth + 1)) ptrs
              PAPClosure{payload = ptrs} -> do
                let ptrAddrs = map boxAddr ptrs
                    node = HeapNode "pap" Nothing ptrAddrs
                atomically $ modifyTVar' nodesRef (Map.insert addr node)
                mapM_ (\p -> walkNode nodesRef boxMapRef visitedRef p (depth + 1)) ptrs
              APClosure{payload = ptrs} -> do
                let ptrAddrs = map boxAddr ptrs
                    node = HeapNode "ap" Nothing ptrAddrs
                atomically $ modifyTVar' nodesRef (Map.insert addr node)
                mapM_ (\p -> walkNode nodesRef boxMapRef visitedRef p (depth + 1)) ptrs
              SelectorClosure{selectee = ptr} -> do
                let node = HeapNode "selector" Nothing [boxAddr ptr]
                atomically $ modifyTVar' nodesRef (Map.insert addr node)
                walkNode nodesRef boxMapRef visitedRef ptr (depth + 1)
              MutVarClosure{var = ptr} -> do
                let node = HeapNode "mutvar" Nothing [boxAddr ptr]
                atomically $ modifyTVar' nodesRef (Map.insert addr node)
                walkNode nodesRef boxMapRef visitedRef ptr (depth + 1)
              _ -> do
                let node = HeapNode "other" (Just (T.pack (take 80 (show closure')))) []
                atomically $ modifyTVar' nodesRef (Map.insert addr node)

  -- Try to extract a displayable value from a cons head pointer
  getHeadValue :: Box -> IO (Maybe Text)
  getHeadValue box = do
    c <- getBoxClosure box
    pure $ case c of
      ConstrClosure{dataArgs = (v : _), name = n}
        | n == "I#" -> Just (T.pack (show v))
        | n == "W#" -> Just (T.pack (show v))
        | n == "C#" -> Just (T.pack (show (toEnum (fromIntegral v) :: Char)))
        | otherwise -> Just (T.pack n <> " " <> T.pack (show v))
      _ -> Nothing

-- Force a thunk by its Box address
forceThunk :: Text -> IO ()
forceThunk addr = do
  boxMap <- readTVarIO globalBoxMap
  case Map.lookup addr boxMap of
    Nothing -> putStrLn $ "Box not found: " <> T.unpack addr
    Just (Box a) -> do
      _ <-
        (evaluate a >> pure ())
          `catch` \(e :: SomeException) ->
            putStrLn $ "Exception while forcing: " <> show e
      pure ()

-- Render the heap as an HTML table
renderHeapTable :: Bool -> Text -> Text -> Map Text HeapNode -> Text
renderHeapTable showRunBtn exprDesc rootAddr nodes =
  "<div id='main' class='max-w-5xl mx-auto'>"
    <> "<div class='flex items-center justify-between mb-6'>"
    <> "<h1 class='text-2xl font-bold text-gray-900 dark:text-gray-100'>GHC Heap Visualizer</h1>"
    <> "<div class='flex gap-2'>"
    <> runButton
    <> "<button data-on:click=\"@get('/reset')\" "
    <> "class='px-4 py-2 bg-gray-200 dark:bg-gray-700 hover:bg-gray-300 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-200 rounded-lg text-sm font-medium cursor-pointer'>"
    <> "Reset</button>"
    <> "<button data-on:click='$dark = !$dark' "
    <> "class='px-4 py-2 bg-gray-200 dark:bg-gray-700 hover:bg-gray-300 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-200 rounded-lg text-sm font-medium cursor-pointer'>"
    <> "<span data-show='$dark'>Light</span>"
    <> "<span data-show='!$dark'>Dark</span>"
    <> "</button>"
    <> "<button data-on:click=\"@get('/heap')\" "
    <> "class='px-4 py-2 bg-gray-200 dark:bg-gray-700 hover:bg-gray-300 dark:hover:bg-gray-600 text-gray-700 dark:text-gray-200 rounded-lg text-sm font-medium cursor-pointer'>"
    <> "Refresh</button>"
    <> "</div></div>"
    <> "<div class='text-sm text-gray-500 mb-4'>Expression: <code class='text-gray-600 dark:text-gray-400'>"
    <> exprDesc
    <> "</code>"
    <> " &middot; Root: <code class='text-gray-600 dark:text-gray-400'>"
    <> cleanAddr rootAddr
    <> "</code>"
    <> " &middot; "
    <> T.pack (show (Map.size nodes))
    <> " nodes</div>"
    <> "<div class='overflow-x-auto rounded-xl border border-gray-200 dark:border-gray-800'>"
    <> "<table class='w-full text-sm'>"
    <> "<thead><tr class='bg-gray-100 dark:bg-gray-900 text-gray-500 dark:text-gray-400 text-left'>"
    <> "<th class='px-4 py-3 font-medium'>Address</th>"
    <> "<th class='px-4 py-3 font-medium'>Type</th>"
    <> "<th class='px-4 py-3 font-medium'>Value</th>"
    <> "<th class='px-4 py-3 font-medium'>Pointers</th>"
    <> "<th class='px-4 py-3 font-medium'>Actions</th>"
    <> "</tr></thead><tbody>"
    <> mconcat (map renderRow orderedNodes)
    <> "</tbody></table></div></div>"
 where
  runButton
    | showRunBtn =
        "<button data-on:click=\"@get('/run')\" "
          <> "class='px-4 py-2 bg-emerald-600 hover:bg-emerald-700 text-white rounded-lg text-sm font-medium cursor-pointer'>"
          <> "Run Demo</button>"
    | otherwise = ""

  -- Root first, then the rest sorted by address
  orderedNodes =
    let rootNode = case Map.lookup rootAddr nodes of
          Just n -> [(rootAddr, n)]
          Nothing -> []
        rest = Map.toAscList (Map.delete rootAddr nodes)
     in rootNode <> rest

  renderRow (addr, node) =
    let isRoot = addr == rootAddr
        ca = cleanAddr addr
        rowClass =
          if isRoot
            then "bg-blue-50 dark:bg-gray-800/50 border-l-2 border-blue-500"
            else "border-b border-gray-200/50 dark:border-gray-800/50 hover:bg-gray-50 dark:hover:bg-gray-800/30"
        highlight =
          " data-class=\"{"
            <> "'ring-2 ring-inset ring-cyan-400 dark:ring-cyan-500 bg-cyan-50 dark:bg-cyan-900/30': $highlight === '"
            <> ca
            <> "'"
            <> "}\""
     in "<tr id='addr-"
          <> ca
          <> "' class='"
          <> rowClass
          <> "'"
          <> highlight
          <> ">"
          <> "<td class='px-4 py-2.5 font-mono text-xs text-gray-500'>"
          <> ca
          <> (if isRoot then " <span class='text-blue-500 dark:text-blue-400 text-xs font-sans'>(root)</span>" else "")
          <> "</td>"
          <> "<td class='px-4 py-2.5'>"
          <> typeBadge (nodeType node)
          <> "</td>"
          <> "<td class='px-4 py-2.5 font-mono'>"
          <> maybe "<span class='text-gray-400 dark:text-gray-600'>-</span>" (\v -> "<span class='text-gray-800 dark:text-gray-200'>" <> v <> "</span>") (nodeValue node)
          <> "</td>"
          <> "<td class='px-4 py-2.5 font-mono text-xs'>"
          <> renderPointers (nodePointers node)
          <> "</td>"
          <> "<td class='px-4 py-2.5'>"
          <> renderActions addr node
          <> "</td>"
          <> "</tr>"

  cleanAddr addr = T.replace "/1" "" (T.replace "/2" "" addr)

  typeBadge ty
    | ty == "cons" = badge "bg-blue-100 text-blue-700 dark:bg-blue-900 dark:text-blue-300" ":"
    | ty == "nil" = badge "bg-gray-100 text-gray-600 dark:bg-gray-700 dark:text-gray-400" "[]"
    | ty == "thunk" = badge "bg-amber-100 text-amber-700 dark:bg-amber-900 dark:text-amber-300" "thunk"
    | ty == "function" = badge "bg-red-100 text-red-700 dark:bg-red-900 dark:text-red-300" "fun"
    | ty == "pap" = badge "bg-orange-100 text-orange-700 dark:bg-orange-900 dark:text-orange-300" "PAP"
    | ty == "ap" = badge "bg-yellow-100 text-yellow-700 dark:bg-yellow-900 dark:text-yellow-300" "AP"
    | ty == "selector" = badge "bg-teal-100 text-teal-700 dark:bg-teal-900 dark:text-teal-300" "sel"
    | ty == "other" = badge "bg-gray-100 text-gray-600 dark:bg-gray-700 dark:text-gray-400" "other"
    | "constr:GHC.Types.I#" `T.isPrefixOf` ty = badge "bg-purple-100 text-purple-700 dark:bg-purple-900 dark:text-purple-300" "I#"
    | "constr:" `T.isPrefixOf` ty = badge "bg-emerald-100 text-emerald-700 dark:bg-emerald-900 dark:text-emerald-300" (T.drop 7 ty)
    | otherwise = badge "bg-gray-100 text-gray-600 dark:bg-gray-700 dark:text-gray-400" ty

  badge cls label =
    "<span class='inline-block px-2 py-0.5 rounded text-xs font-medium "
      <> cls
      <> "'>"
      <> label
      <> "</span>"

  renderPointers [] = "<span class='text-gray-400 dark:text-gray-600'>-</span>"
  renderPointers ptrs = T.intercalate " " (map renderPtr ptrs)

  renderPtr p =
    let cp = cleanAddr p
     in "<a href='#addr-"
          <> cp
          <> "'"
          <> " data-on:mouseenter=\"$highlight = '"
          <> cp
          <> "'\""
          <> " data-on:mouseleave=\"$highlight = ''\""
          <> " class='text-cyan-700 dark:text-cyan-600 hover:underline cursor-pointer'>"
          <> cp
          <> "</a>"

  renderActions addr node
    | nodeType node == "thunk" || nodeType node == "ap" || nodeType node == "selector" =
        "<button data-on:click=\"@get('/force?addr="
          <> addr
          <> "')\" "
          <> "class='px-3 py-1 bg-amber-200 hover:bg-amber-300 text-amber-800 dark:bg-amber-800 dark:hover:bg-amber-700 dark:text-amber-200 rounded text-xs font-medium cursor-pointer'>"
          <> "Force</button>"
    | otherwise = ""

-- Application

main :: IO ()
main = do
  args <- getArgs
  case args of
    [name] | Just mode <- lookup name allModes -> startServer 3000 mode
    [name, portStr] | Just mode <- lookup name allModes -> startServer (read portStr) mode
    _ -> do
      hPutStrLn stderr "Usage: heap-view <mode> [port]"
      hPutStrLn stderr ""
      hPutStrLn stderr "Modes:"
      mapM_ (\(name, mode) -> hPutStrLn stderr $ "  " <> name <> replicate (16 - length name) ' ' <> T.unpack (modeDesc mode)) allModes
      exitFailure

startServer :: Int -> Mode -> IO ()
startServer port mode = do
  htmlContent <- BS.readFile "examples/heap-view.html"
  modeSetup mode
  let hasRun = case modeRun mode of Just _ -> True; Nothing -> False
  putStrLn $ "GHC Heap Visualizer [" <> T.unpack (modeName mode) <> "]"
  putStrLn $ "Listening on http://localhost:" <> show port
  Warp.run port (app htmlContent mode hasRun)

app :: BS.ByteString -> Mode -> Bool -> Application
app htmlContent mode hasRun req respond =
  case (requestMethod req, pathInfo req) of
    ("GET", []) ->
      respond $ responseLBS status200 [("Content-Type", "text/html")] (LBS.fromStrict htmlContent)
    ("GET", ["heap"]) ->
      handleHeap hasRun respond
    ("GET", ["force"]) ->
      handleForce hasRun req respond
    ("GET", ["run"])
      | Just run <- modeRun mode ->
          handleRun hasRun run respond
    ("GET", ["reset"]) ->
      handleReset hasRun mode respond
    _ ->
      respond $ responseLBS status404 [] "Not found"

sendHeapUpdate :: Bool -> ServerSentEventGenerator -> IO ()
sendHeapUpdate showRunBtn gen = do
  box <- readTVarIO currentExpression
  desc <- readTVarIO currentExprDesc
  performGC
  (rootAddr, nodes) <- walkHeap box 20
  let html = renderHeapTable showRunBtn desc rootAddr nodes
  sendPatchElements gen (patchElements html)

handleHeap :: Bool -> (Response -> IO b) -> IO b
handleHeap hasRun respond =
  respond $ sseResponse $ \gen ->
    sendHeapUpdate hasRun gen

handleForce :: Bool -> Request -> (Response -> IO b) -> IO b
handleForce hasRun req respond = do
  let params = queryToQueryText (queryString req)
  case lookup "addr" params of
    Just (Just addr) -> do
      forceThunk addr
      respond $ sseResponse $ \gen ->
        sendHeapUpdate hasRun gen
    _ ->
      respond $ responseLBS status400 [] "Missing addr parameter"

handleReset :: Bool -> Mode -> (Response -> IO b) -> IO b
handleReset hasRun mode respond = do
  modeSetup mode
  respond $ sseResponse $ \gen ->
    sendHeapUpdate hasRun gen

handleRun :: Bool -> IO () -> (Response -> IO b) -> IO b
handleRun hasRun run respond = do
  run
  respond $ sseResponse $ \gen -> do
    -- Stream live updates every 200ms for ~12 seconds
    replicateM_ 60 $ do
      sendHeapUpdate hasRun gen
      threadDelay 200000
