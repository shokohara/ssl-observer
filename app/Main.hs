{-# LANGUAGE OverloadedStrings #-}

module Main where
import Network.HTTP.Conduit as C
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe
import Data.ByteString.Char8
import Data.Aeson
import Control.Applicative
import Control.Monad
import System.Environment
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

data Payload = Payload { test :: String } deriving Show
instance FromJSON Payload where
  parseJSON (Object v) = Payload <$> (v .: "text")
instance ToJSON Payload where
  toJSON (Payload text) = object [ "text" .= text ]

url = "https://hooks.slack.com/services/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"
domain = "google.com"
day = 3

sendMessage :: String -> IO ()
sendMessage text = do
  initReq <- parseUrlThrow url
  let payload = Payload text
  let req' = initReq { secure = True, method = "POST" } -- Turn on https
  let req = urlEncodedBody [ ("payload", L.toStrict $ encode payload) ] req'
  response <- withManager $ httpLbs req
  L.putStr $ responseBody response

main = do
  (_, x, _) <- readProcessWithExitCode "sh" ["-c", "gdate -d\"$(echo '' | openssl s_client -connect " ++ domain ++ ":443 -servername " ++ domain ++" 3> /dev/null | openssl x509 -enddate -noout | sed 's/notAfter=//')\" --utc --iso-8601=\"seconds\""] []
  z <- maybeToIO $ parseISO8601 x
  c <- getCurrentTime
  when (diffUTCTime z c < realToFrac 60*60*24*day) $ sendMessage domain
