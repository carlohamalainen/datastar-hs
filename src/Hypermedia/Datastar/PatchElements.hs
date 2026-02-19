module Hypermedia.Datastar.PatchElements where

import Data.Text (Text)
import Data.Text qualified as T
import Hypermedia.Datastar.Types

data PatchElements = PatchElements
  { peElements :: Maybe Text
  , peSelector :: Maybe Text
  , peMode :: ElementPatchMode
  , peUseViewTransition :: Bool
  , peNamespace :: ElementNamespace
  , peEventId :: Maybe Text
  , peRetryDuration :: Int
  }
  deriving (Eq, Show)

patchElements :: Text -> PatchElements
patchElements html =
  PatchElements
    { peElements = if T.null html then Nothing else Just html
    , peSelector = Nothing
    , peMode = defaultPatchMode
    , peUseViewTransition = defaultUseViewTransition
    , peNamespace = defaultNamespace
    , peEventId = Nothing
    , peRetryDuration = defaultRetryDuration
    }

removeElements :: Text -> PatchElements
removeElements sel =
  PatchElements
    { peElements = Nothing
    , peSelector = Just sel
    , peMode = defaultPatchMode
    , peUseViewTransition = defaultUseViewTransition
    , peNamespace = defaultNamespace
    , peEventId = Nothing
    , peRetryDuration = defaultRetryDuration
    }

-- FIXME link to ADR - fields only need to be included if non-default etc

toDatastarEvent :: PatchElements -> DatastarEvent
toDatastarEvent pe =
  DatastarEvent
    { eventType = EventPatchElements
    , eventId = peEventId pe
    , retry = peRetryDuration pe
    , dataLines =
        concat
          [ maybe [] (\s -> ["selector " <> s]) (peSelector pe)
          , [ "mode " <> patchModeToText (peMode pe)
            | peMode pe /= defaultPatchMode
            ]
          , [ "useViewTransition true"
            | peUseViewTransition pe
            ]
          , [ "namespace " <> namespaceToText (peNamespace pe)
            | peNamespace pe /= defaultNamespace
            ]
          , maybe [] (map ("elements " <>) . T.lines) (peElements pe)
          ]
    }
