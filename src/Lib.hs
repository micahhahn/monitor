{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Lib where

import Control.Monad (forever)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as ByteString
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import GHC.Generics
import System.Process as Process

import Data.Text.Encoding as Encoding

import qualified Network.HTTP.Conduit as Http
import Network.HTTP.Client.Conduit (defaultManagerSettings)

import qualified Network.URI as Uri
import Network.WebSockets as WS
import qualified Network.WebSockets.Client as WS

data ChromiumPageInfo = ChromiumPageInfo
    { _description :: Text
    , _devtoolsFrontendUrl :: Text
    , _id :: Text
    , _title :: Text
    , _type :: Text
    , _url :: Text
    , _webSocketDebuggerUrl :: Text
    } deriving (Show, Generic)

instance FromJSON ChromiumPageInfo where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 1 }

maybeWithError :: Text -> Maybe a -> Either Text a
maybeWithError t Nothing = Left t
maybeWithError _ (Just a) = Right a

loadChromiumUri :: Int -> IO (Either Text (String, Int, String))
loadChromiumUri port = do
    response <- Http.simpleHttp $ "http://localhost:" ++ show port ++ "/json"
    return $ case eitherDecode response of
        Left s -> Left . Text.pack $ s
        Right [] -> Left "No chromium found."
        Right (a:b:cs) -> Left "Why so many?"
        Right [pageInfo] -> maybeWithError "Failed to parse uri" (do
            uri <- Uri.parseURI (Text.unpack . _webSocketDebuggerUrl $ pageInfo)
            auth <- Uri.uriAuthority uri
            let port = case Uri.uriPort auth of 
                           (':' : str) -> read str 
                           _ -> 80
            return (Uri.uriRegName auth, port, Uri.uriPath uri))

withChromium :: (Int -> IO a) -> IO a
withChromium f = do
    handle <- Process.spawnProcess "C:\\Program Files (x86)\\Google\\Chrome\\Application\\chrome.exe" ["--headless", "--remote-debugging-port=9222"]
    ret <- f 9222
    terminateProcess handle
    return ret

data Command = Command
    { _id :: Int
    , _method :: Text
    , _params :: HashMap Text Text
    } deriving (Show, Generic)

instance ToJSON Command where
    toJSON = genericToJSON defaultOptions { fieldLabelModifier = drop 1 }
 
runTest :: IO ()
runTest = {- withChromium $ \port ->-} do
    eitherUri <- loadChromiumUri 9222
    case eitherUri of
        Left t -> TextIO.putStrLn t
        Right (host, port', path) -> do
            WS.runClient "localhost" port' path $ \conn -> do
                let command = encode $ Command
                                        { _id = 1
                                        , _method = "Page.navigate"
                                        , _params = HashMap.fromList [("url", "https://haskell.org")]
                                        }

                WS.sendTextData conn command
                msg <- WS.receiveData conn
                TextIO.putStrLn msg

                let c2 = encode $ Command
                                    { _id = 2
                                    , _method = "DOM.getDocument"
                                    , _params = HashMap.empty
                                    }
                                
                WS.sendTextData conn c2
                msg <- WS.receiveData conn
                TextIO.putStrLn msg

                
    return ()

someFunc :: IO ()
someFunc = undefined  