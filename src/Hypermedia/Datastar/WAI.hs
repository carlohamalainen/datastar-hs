module Hypermedia.Datastar.WAI where

import Control.Concurrent.MVar
import Control.Exception
import Control.Lens

import Data.Text (Text)
import Data.Text.Encoding qualified as TE

import Data.Aeson (FromJSON)
import Data.Aeson qualified as A

import Data.ByteString.Builder qualified as BSB

import Network.HTTP.Types qualified as WAI
import Network.Wai qualified as WAI

import Hypermedia.Datastar.Types

import Hypermedia.Datastar.ExecuteScript qualified as ES
import Hypermedia.Datastar.PatchElements qualified as PE
import Hypermedia.Datastar.PatchSignals qualified as PS

data ServerSentEventGenerator = ServerSentEventGenerator
  { _sseWrite :: BSB.Builder -> IO ()
  , _sseFlush :: IO ()
  , _sseLock :: MVar ()
  -- , _sseLogger :: DatastarLogger -- FIXME
  }

makeLenses ''ServerSentEventGenerator

sseResponse :: (ServerSentEventGenerator -> IO ()) -> WAI.Response
sseResponse callback =
  WAI.responseStream
    WAI.status200
    headers
    action
 where
  headers =
    [ ("Cache-Control", "no-cache")
    , ("Content-Type", "text/event-stream")
    , ("Connection", "keep-alive")
    ]

  action write flush = do
    lock <- newMVar ()
    callback $
      ServerSentEventGenerator
        { _sseWrite = write
        , _sseFlush = flush
        , _sseLock = lock
        }

send :: ServerSentEventGenerator -> DatastarEvent -> IO ()
send gen event = do
  let rendered = renderEvent event

  bracket_
    (takeMVar $ gen ^. sseLock)
    (putMVar (gen ^. sseLock) ())
    $ do
      (gen ^. sseWrite) rendered
      gen ^. sseFlush

-- FIXME run logger for send event
-- FIXME exceptions? How safe is this?

sendPatchElements :: ServerSentEventGenerator -> PE.PatchElements -> IO ()
sendPatchElements gen pe = send gen $ PE.toDatastarEvent pe

sendPatchSignals :: ServerSentEventGenerator -> PS.PatchSignals -> IO ()
sendPatchSignals gen ps = send gen $ PS.toDatastarEvent ps

sendExecuteScript :: ServerSentEventGenerator -> ES.ExecuteScript -> IO ()
sendExecuteScript gen es = send gen $ ES.toDatastarEvent es

readSignals :: (FromJSON a) => WAI.Request -> IO (Either String a)
readSignals req
  | WAI.requestMethod req == "GET" =
      pure $ parseFromQuery req
  | otherwise =
      parseFromBody req

parseFromQuery :: (FromJSON a) => WAI.Request -> Either String a
parseFromQuery req =
  case lookup "datastar" (WAI.queryString req) of
    (Just (Just val)) ->
      A.eitherDecodeStrict $ WAI.urlDecode True val
    _ -> Left "missing 'datastar' query parameter"

parseFromBody :: (FromJSON a) => WAI.Request -> IO (Either String a)
parseFromBody req = A.eitherDecode <$> WAI.strictRequestBody req

isDatastarRequest :: WAI.Request -> Bool
isDatastarRequest = has $ to WAI.requestHeaders . folded . _1 . only "datastar-request"

renderEvent :: DatastarEvent -> BSB.Builder
renderEvent event =
  mconcat
    [ BSB.stringUtf8 "event: " <> text (event ^. eventType . to eventTypeToText) <> newline
    , event ^. eventId . _Just . to (\eid -> BSB.stringUtf8 "id: " <> text eid <> newline)
    , if event ^. retry /= defaultRetryDuration
        then BSB.stringUtf8 "retry: " <> BSB.intDec (event ^. retry) <> newline
        else mempty
    , event ^. dataLines . folded . to (\line -> BSB.stringUtf8 "data: " <> text line <> newline)
    , newline
    ]
 where
  text :: Text -> BSB.Builder
  text = TE.encodeUtf8Builder

  newline :: BSB.Builder
  newline = BSB.charUtf8 '\n'
