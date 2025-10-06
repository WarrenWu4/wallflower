module LoggerGe
  ( LogLevel(..),
    logMsg,
  )
where

import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)
import Data.GI.Base.CallStack (CallStack, HasCallStack, callStack)
import Data.Foldable (find)
import GHC.Exception (SrcLoc(srcLocFile, srcLocStartLine), getCallStack)

-- the good enough logger

data LogLevel = INFO | WARNING | ERROR | DEBUG | OK deriving (Show)

ansiReset :: String
ansiReset = "\ESC[0m"

-- | Determines the appropriate ANSI color code based on the LogLevel.
getColorCode :: LogLevel -> String
getColorCode level = case level of
  ERROR -> "\ESC[31m" --  red
  WARNING -> "\ESC[33m" -- yellow
  INFO -> "\ESC[34m" -- blue
  OK -> "\ESC[32m" -- green
  DEBUG -> "\ESC[35m" -- purple (idk)

-- | Helper function to extract the relevant calling location from the stack.
-- It attempts to skip calls originating from within this Logger.hs file.
getCallerLocation :: CallStack -> String
getCallerLocation cs =
  case find (\(_, loc) -> "Logger.hs" /= srcLocFile loc) (getCallStack cs) of
    Just (_, loc) ->
      let
          file = srcLocFile loc
          line = show (srcLocStartLine loc)
       in file ++ ":" ++ line
    Nothing -> "Unknown"

-- | logs a message to standard output
-- based on the following format:
-- [timestamp] [log level] [source] message
logMsg :: HasCallStack => LogLevel -> String -> IO ()
logMsg lt msg = do
  timestamp <- getZonedTime
  let src = getCallerLocation callStack 
  let timeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" timestamp
  let levelStr = show lt
  let logLine = getColorCode lt ++ "[" ++ timeStr ++ "] [" ++ levelStr ++ "] [" ++ src ++ "] " ++ msg ++ ansiReset
  putStrLn logLine
