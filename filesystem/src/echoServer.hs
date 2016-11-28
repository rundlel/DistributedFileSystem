{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}

module echoServer
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Data.Char
import Data.List
import GHC.Generics

data Message = Message
	{ string :: String }
	deriving(Generic)

instance FromJSON Message
instance ToJSON Message

type API = "message" :> Capture "input" String :> Get '[JSON] Message


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = sendMessage

sendMessage :: Server API
sendMessage = sendEchoMessage where
	sendEchoMessage :: String -> Handler Message
	sendEchoMessage m = return (Message (map toUpper m))