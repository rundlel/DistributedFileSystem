{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Data.List
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import System.IO
import Database.MongoDB
import Control.Monad.Trans (liftIO)

data Message = Message
 	{ message :: String }
 	deriving (Generic)


data SendFile = SendFile
	{ sendFile :: String }
	deriving (Generic)

instance FromJSON Message
instance ToJSON Message


type API = "file" :> Get 
		-- "message" :> Capture "in" String :> Get '[JSON] Message 
		-- :<|> 


startApp :: IO ()
startApp = run 8080 app
	

fileReader :: IO ()
fileReader = do
	sendFile <- openFile "text.txt" ReadMode
	inpString <- hGetContents sendFile
	let words = processData inpString
	return (SendFile (map toUpper words))
	hClose sendFile

processData :: String -> String
processData = map toUpper

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = fileReader
	-- echoMessage

-- echoMessage :: Server API
-- echoMessage = sendEcho where
-- 	sendEcho :: String -> Handler Message
--  	sendEcho s = return (Message (map toUpper s))



-- startApp :: IO ()
-- startApp = do
	-- handle <- openFile "text.txt" ReadMode
	-- contents <- hGetContents handle
	-- putStr contents
	-- hClose handle



