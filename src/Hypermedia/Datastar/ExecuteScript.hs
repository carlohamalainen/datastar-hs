module Hypermedia.Datastar.ExecuteScript where

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

data ExecuteScript = ExecuteScript
  { esScript :: Text
  , esAutoRemove :: Bool
  , esAttributes :: [Text]
  , esEventId :: Maybe Text
  , esRetryDuration :: Int
  }
  deriving (Eq, Show)

executeScript :: Text -> ExecuteScript
executeScript js =
  ExecuteScript
    { esScript = js
    , esAutoRemove = defaultAutoRemove
    , esAttributes = []
    , esEventId = Nothing
    , esRetryDuration = defaultRetryDuration
    }

toDatastarEvent :: ExecuteScript -> DatastarEvent
toDatastarEvent es =
  DatastarEvent
    { eventType = EventPatchElements -- Correct, there is no EventExecuteScript, see the ADR
    , eventId = esEventId es
    , retry = esRetryDuration es
    , dataLines =
        [ "selector body"
        , "mode append"
        ]
          <> buildScriptLines es
    }

buildScriptLines :: ExecuteScript -> [Text]
buildScriptLines es =
  case T.lines (esScript es) of
    [] -> ["elements " <> openTag <> closeTag]
    [single] -> ["elements " <> openTag <> single <> closeTag]
    multiple ->
      ["elements " <> openTag]
        <> map ("elements " <>) multiple
        <> ["elements " <> closeTag]
 where
  openTag :: Text
  openTag =
    "<script"
      <> (if esAutoRemove es then " data-effect=\"el.remove()\"" else "")
      <> foldMap (" " <>) (esAttributes es)
      <> ">"

  closeTag :: Text
  closeTag = "</script>"
