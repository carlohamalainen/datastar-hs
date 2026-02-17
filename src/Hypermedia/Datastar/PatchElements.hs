module Hypermedia.Datastar.PatchElements where

import Control.Lens

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

data PatchElements = PatchElements
  { _peElements :: Maybe Text
  , _peSelector :: Maybe Text
  , _peMode :: ElementPatchMode
  , _peUseViewTransition :: Bool
  , _peNamespace :: ElementNamespace
  , _peEventId :: Maybe Text
  , _peRetryDuration :: Int
  }
  deriving (Eq, Show)

makeLenses ''PatchElements

patchElements :: Text -> PatchElements
patchElements html =
  PatchElements
    { _peElements = if T.null html then Nothing else Just html
    , _peSelector = Nothing
    , _peMode = defaultPatchMode
    , _peUseViewTransition = defaultUseViewTransition
    , _peNamespace = defaultNamespace
    , _peEventId = Nothing
    , _peRetryDuration = defaultRetryDuration
    }

removeElements :: Text -> PatchElements
removeElements sel =
  PatchElements
    { _peElements = Nothing
    , _peSelector = Just sel
    , _peMode = defaultPatchMode
    , _peUseViewTransition = defaultUseViewTransition
    , _peNamespace = defaultNamespace
    , _peEventId = Nothing
    , _peRetryDuration = defaultRetryDuration
    }

-- FIXME link to ADR - fields only need to be included if non-default etc

toDatastarEvent :: PatchElements -> DatastarEvent
toDatastarEvent pe =
  DatastarEvent
    { _eventType = EventPatchElements
    , _eventId = pe ^. peEventId
    , _retry = pe ^. peRetryDuration
    , _dataLines =
        concat
          [ pe ^. peSelector . _Just . to (pure . ("selector " <>))
          , [ "mode " <> patchModeToText (pe ^. peMode)
            | pe ^. peMode /= defaultPatchMode
            ]
          , [ "useViewTransition true"
            | pe ^. peUseViewTransition
            ]
          , [ "namespace " <> namespaceToText (pe ^. peNamespace)
            | pe ^. peNamespace /= defaultNamespace
            ]
          , pe ^. peElements . _Just . to T.lines . to (map ("elements " <>))
          ]
    }
