-- | Debug Options
module Graphics.QML.Debug (
    setDebugLogLevel
) where

import Graphics.QML.Internal.Engine

-- | Sets the global debug log level. At level zero, no logging information
-- will be printed. Higher levels will increase debug verbosity.
setDebugLogLevel :: Int -> IO ()
setDebugLogLevel = hsqmlSetDebugLoglevel
