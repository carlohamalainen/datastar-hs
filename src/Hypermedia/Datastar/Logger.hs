module Hypermedia.Datastar.Logger
  ( DatastarLogger (..)
  , nullLogger
  , stderrLogger
  )
where

import Control.Lens
import Data.Text (Text)
import Data.Text qualified as T
import System.IO (hPutStrLn, stderr)

data DatastarLogger = DatastarLogger
  { _logDebug :: Text -> IO ()
  , _logInfo :: Text -> IO ()
  , _logWarn :: Text -> IO ()
  , _logError :: Text -> IO ()
  }

makeLenses ''DatastarLogger

nullLogger :: DatastarLogger
nullLogger =
  DatastarLogger
    { _logDebug = const (pure ())
    , _logInfo = const (pure ())
    , _logWarn = const (pure ())
    , _logError = const (pure ())
    }

stderrLogger :: DatastarLogger
stderrLogger =
  DatastarLogger
    { _logDebug = logAt "DEBUG"
    , _logInfo = logAt "INFO"
    , _logWarn = logAt "WARN"
    , _logError = logAt "ERROR"
    }
 where
  logAt :: Text -> Text -> IO ()
  logAt level msg = hPutStrLn stderr $ T.unpack ("[datastar] [" <> level <> "] " <> msg)
