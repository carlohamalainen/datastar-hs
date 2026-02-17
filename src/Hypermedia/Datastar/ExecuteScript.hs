module Hypermedia.Datastar.ExecuteScript where

import Control.Lens

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

data ExecuteScript = ExecuteScript
  { _esScript :: Text
  , _esAutoRemove :: Bool
  , _esAttributes :: [Text]
  , _esEventId :: Maybe Text
  , _esRetryDuration :: Int
  }
  deriving (Eq, Show)

makeLenses ''ExecuteScript

executeScript :: Text -> ExecuteScript
executeScript js =
  ExecuteScript
    { _esScript = js
    , _esAutoRemove = defaultAutoRemove
    , _esAttributes = []
    , _esEventId = Nothing
    , _esRetryDuration = defaultRetryDuration
    }

toDatastarEvent :: ExecuteScript -> DatastarEvent
toDatastarEvent es =
  DatastarEvent
    { _eventType = EventPatchElements -- Correct, there is no EventExecuteScript, see the ADR
    , _eventId = es ^. esEventId
    , _retry = es ^. esRetryDuration
    , _dataLines =
        [ "selector body"
        , "mode append"
        ]
          <> buildScriptLines es
    }

buildScriptLines :: ExecuteScript -> [Text]
buildScriptLines es =
  case es ^. esScript . to T.lines of
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
      <> (if es ^. esAutoRemove then " data-effect=\"el.remove()\"" else "")
      <> es ^. esAttributes . folded . to (" " <>)
      <> ">"

  closeTag :: Text
  closeTag = "</script>"
