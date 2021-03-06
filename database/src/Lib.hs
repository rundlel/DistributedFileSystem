{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Lib
    ( startApp
    ) where

import Data.Aeson
import Data.Aeson.TH
import Data.Char
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import GHC.Generics
import System.IO
import Database.MongoDB (Action, Document, Value,
                                        access, allCollections, close, connect, delete,
                                        exclude, find, insert, findOne, host, insertMany,
                                        master, project, rest, select, sort,
                                        (=:))
import Control.Monad.Trans (liftIO)
import Data.Bson.Generic
import Control.Concurrent.MVar

data Message = Message
 	{ message :: String }
 	deriving (Generic)

data ResponseData = ResponseData
	{ responseData :: String }
	deriving(Generic)

data SendFile = SendFile
	{ fileContents :: String }
	deriving (Generic, FromBSON, ToBSON, FromJSON, ToJSON)

instance FromJSON ResponseData
instance ToJSON ResponseData

deriving instance ToBSON String
deriving instance FromBSON String

type API = "postFile" :> ReqBody '[JSON] SendFile :> Post '[JSON] ResponseData


startApp :: IO ()
startApp = run 8080 app
	

-- fileReader :: IO ()
-- fileReader = do
-- 	sendFile <- openFile "text.txt" ReadMode
-- 	inpString <- hGetContents sendFile
-- 	let words = processData inpString
-- 	return (SendFile (map toUpper words))
-- 	hClose sendFile

processData :: String -> String
processData = map toUpper

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = postFile

-- echoMessage :: Server API
-- echoMessage = sendEcho where
-- 	sendEcho :: String -> Handler Message
--  	sendEcho s = return (Message (map toUpper s))

startMongoDB functionToRun = do
	pipe <- connect (host "127.0.0.1")
	e <- access pipe master "LFSdatabase" functionToRun
	print e
	close pipe

printDBCollecs = startMongoDB allCollections

printDBFiles = startMongoDB $ find (select [] "Files") >>= rest

insertFileToDatabase :: Document -> IO()
insertFileToDatabase docToInsert = startMongoDB $ insert "Files" docToInsert

postFile :: SendFile -> Handler ResponseData
postFile doc = liftIO $ do
	x <- insertFileToDatabase $ (toBSON $ doc)
	return $ ResponseData (fileContents doc)
	--handle <- openFile "text.txt" ReadMode
	--contents <- hGetContents handle
	--let textFileContent = words contents
	--startMongoDB $ insert "Files" $ toBSON textFileContent
	--hClose handle


-- startApp :: IO ()
-- startApp = do
	-- handle <- openFile "text.txt" ReadMode
	-- contents <- hGetContents handle
	-- putStr contents
	-- hClose handle

--withFile "text.txt" ReadMode (\handle -> do
--		contents <- hGetContents handle
--		startMongoDB $ insert "Files" toBSON textFileContent)



-- {-# LANGUAGE DataKinds       #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeOperators   #-}
-- module Lib
--     ( startApp
--     ) where

-- import Data.Aeson
-- import Data.Aeson.TH
-- import Network.Wai
-- import Network.Wai.Handler.Warp
-- import Servant
-- import

-- data User = User
--   { userId        :: Int
--   , userFirstName :: String
--   , userLastName  :: String
--   } deriving (Eq, Show)

-- $(deriveJSON defaultOptions ''User)

-- type API = "users" :> Get '[JSON] [User]

-- startApp :: IO ()
-- startApp = run 8080 app

-- app :: Application
-- app = serve api server

-- api :: Proxy API
-- api = Proxy

-- server :: Server API
-- server = return users

-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         ]
