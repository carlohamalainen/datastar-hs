module Hypermedia.Datastar.PatchSignals where

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

data PatchSignals = PatchSignals
  { psSignals :: Text
  , psOnlyIfMissing :: Bool
  , psEventId :: Maybe Text
  , psRetryDuration :: Int
  }
  deriving (Eq, Show)

patchSignals :: Text -> PatchSignals
patchSignals sigs =
  PatchSignals
    { psSignals = sigs
    , psOnlyIfMissing = defaultOnlyIfMissing
    , psEventId = Nothing
    , psRetryDuration = defaultRetryDuration
    }

toDatastarEvent :: PatchSignals -> DatastarEvent
toDatastarEvent ps =
  DatastarEvent
    { eventType = EventPatchSignals
    , eventId = psEventId ps
    , retry = psRetryDuration ps
    , dataLines = ifMissingLine ++ signalLines
    }
 where
  ifMissingLine = ["onlyIfMissing true" | psOnlyIfMissing ps]
  signalLines = map ("signals " <>) $ T.lines $ psSignals ps
