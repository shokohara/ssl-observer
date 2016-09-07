module Main where

import Lib
import System.Process
import Data.Time.ISO8601
import Data.Time.Clock
import Data.Maybe
import Control.Exception
import Data.Typeable

data NoString = NoString deriving (Typeable, Show)
instance Exception NoString

maybeToIO Nothing = throwIO NoString
maybeToIO (Just x) = return x

main = do
  (_, x, _) <- readProcessWithExitCode "sh" ["-c", "gdate -d\"$(gdate -d\"$(echo '' | openssl s_client -connect google.com:443 -servername google.com 3> /dev/null | openssl x509 -enddate -noout | sed 's/notAfter=//')\")\" --utc --iso-8601=\"seconds\" "] []
  z <- maybeToIO $ parseISO8601 x
  c <- getCurrentTime
  let d = diffUTCTime z c
  print $ d < realToFrac 60*60*24*30*14
