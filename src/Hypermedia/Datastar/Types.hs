module Hypermedia.Datastar.Types where

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
  { eventType :: EventType
  , eventId :: Maybe Text
  , retry :: Int
  , dataLines :: [Text]
  }

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
