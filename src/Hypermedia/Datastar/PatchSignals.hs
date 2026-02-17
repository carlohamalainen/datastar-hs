module Hypermedia.Datastar.PatchSignals where

import Control.Lens

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

data PatchSignals = PatchSignals
  { _psSignals :: Text
  , _psOnlyIfMissing :: Bool
  , _psEventId :: Maybe Text
  , _psRetryDuration :: Int
  }
  deriving (Eq, Show)

makeLenses ''PatchSignals

patchSignals :: Text -> PatchSignals
patchSignals sigs =
  PatchSignals
    { _psSignals = sigs
    , _psOnlyIfMissing = defaultOnlyIfMissing
    , _psEventId = Nothing
    , _psRetryDuration = defaultRetryDuration
    }

toDatastarEvent :: PatchSignals -> DatastarEvent
toDatastarEvent ps =
  DatastarEvent
    { _eventType = EventPatchSignals
    , _eventId = ps ^. psEventId
    , _retry = ps ^. psRetryDuration
    , _dataLines = ifMissingLine ++ signalLines
    }
 where
  ifMissingLine = ["onlyIfMissing true" | ps ^. psOnlyIfMissing]
  signalLines = map ("signals " <>) $ T.lines $ ps ^. psSignals
