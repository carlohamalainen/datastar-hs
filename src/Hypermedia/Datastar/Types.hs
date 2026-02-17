module Hypermedia.Datastar.Types where

import Control.Lens
import Data.Text (Text)

data EventType
  = EventPatchElements
  | EventPatchSignals
  deriving (Eq, Show)

eventTypeToText :: EventType -> Text
eventTypeToText EventPatchElements = "datastar-patch-elements"
eventTypeToText EventPatchSignals = "datastar-patch-signals"

data ElementPatchMode
  = Outer
  | Inner
  | Remove
  | Replace
  | Prepend
  | Append
  | Before
  | After
  deriving (Eq, Show)

patchModeToText :: ElementPatchMode -> Text
patchModeToText Outer = "outer"
patchModeToText Inner = "inner"
patchModeToText Remove = "remove"
patchModeToText Replace = "replace"
patchModeToText Prepend = "prepend"
patchModeToText Append = "append"
patchModeToText Before = "before"
patchModeToText After = "after"

data ElementNamespace
  = HtmlNs
  | SvgNs
  | MathmlNs
  deriving (Eq, Show)

namespaceToText :: ElementNamespace -> Text
namespaceToText HtmlNs = "html"
namespaceToText SvgNs = "svg"
namespaceToText MathmlNs = "mathml"

data DatastarEvent = DatastarEvent
  { _eventType :: EventType
  , _eventId :: Maybe Text
  , _retry :: Int
  , _dataLines :: [Text]
  }

makeLenses ''DatastarEvent

-- FIXME link to the ADR, these constants are defined there

defaultRetryDuration :: Int
defaultRetryDuration = 1000

defaultPatchMode :: ElementPatchMode
defaultPatchMode = Outer

defaultUseViewTransition :: Bool
defaultUseViewTransition = False

defaultOnlyIfMissing :: Bool
defaultOnlyIfMissing = False

defaultAutoRemove :: Bool
defaultAutoRemove = True

defaultNamespace :: ElementNamespace
defaultNamespace = HtmlNs
