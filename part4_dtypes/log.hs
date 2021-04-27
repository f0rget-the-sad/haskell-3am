import Data.Time.Clock
import Data.Time.Format
import Data.Function 
-- import System.Locale

timeToString :: UTCTime -> String
timeToString = formatTime defaultTimeLocale "%a %d %T"

data LogLevel = Error | Warning | Info deriving Show

data LogEntry = LogEntry {
    timestamp :: UTCTime,
    logLevel :: LogLevel,
    message :: String
}

logLevelToString :: LogLevel -> String
logLevelToString = show

logEntryToString :: LogEntry -> String
logEntryToString log = concat [
    timeToString (log & timestamp), ": ",
    show (log & logLevel), ": ",
    log & message]


test1 =
  let ct = read "2019-02-24 18:28:52.607875 UTC"::UTCTime
      le = LogEntry ct Info "Info Message"
  in logEntryToString le