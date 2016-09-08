{-# LANGUAGE OverloadedStrings #-}

module Main where
import Network.HTTP.Conduit as C
import qualified Data.ByteString.Lazy as L
import System.IO.Unsafe
import Data.ByteString.Char8
import Data.Aeson
import Control.Applicative
import System.Environment
import Lib
import System.Process
import Data.Time.ISO8601
import Data.Time.Clock
import Data.Maybe
import Control.Exception
import Data.Typeable
import Control.Lens
import Data.Aeson.Lens
import Data.Aeson.TH (deriveJSON, defaultOptions, Options(..))
import Data.Char (toLower)
import System.IO.Unsafe

data NoString = NoString deriving (Typeable, Show)
instance Exception NoString

maybeToIO Nothing = throwIO NoString
maybeToIO (Just x) = return x

data Payload = Payload { test :: String } deriving Show
instance FromJSON Payload where
    parseJSON (Object v) = Payload <$> (v .: "text")

instance ToJSON Payload where
    toJSON (Payload text) = object [ "text" Data.Aeson..= text ]

sendMessage :: String -> IO ()
sendMessage text = do
    initReq <- parseUrl "https://hooks.slack.com/services/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"
    let payload = Payload text
    let req' = initReq { secure = True, method = "POST" } -- Turn on https
    let req = (flip urlEncodedBody) req' [ ("payload", L.toStrict $ encode payload) ]
    response <- withManager $ httpLbs req
    L.putStr $ responseBody response

ssl = do
  (_, x, _) <- readProcessWithExitCode "sh" ["-c", "gdate -d\"$(gdate -d\"$(echo '' | openssl s_client -connect google.com:443 -servername google.com 3> /dev/null | openssl x509 -enddate -noout | sed 's/notAfter=//')\")\" --utc --iso-8601=\"seconds\" "] []
  z <- maybeToIO $ parseISO8601 x
  c <- getCurrentTime
  let d = diffUTCTime z c
  print $ d < realToFrac 60*60*24*30*14

main = do
  let json = unsafePerformIO $ Prelude.readFile ".event.json"
  print $ json ^.. key "Records" . _Array . traverse . to ( \o -> ( o^?! key "EventSubscriptionArn" . _String, o^?! key "EventSource" . _String, o^?! key "EventVersion" . _String, o ^.. key "Sns" . key "Subject" . _String ))
