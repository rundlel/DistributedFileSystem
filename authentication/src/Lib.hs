{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
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
import System.IO
import GHC.Generics
import Control.Monad.Trans (liftIO)
import System.Random
import Database.MongoDB (Action, Document, Value,
                                        access, allCollections, close, connect, delete,
                                        exclude, find, insert, findOne, host, insertMany,
                                        master, project, rest, select, sort,
                                        (=:))
import Data.Bson.Generic
import Control.Concurrent.MVar
import Data.Maybe (mapMaybe)


data Token = Token
	{ key :: Key
	, metadata :: String
	} deriving (Show, Read)

data Key = Key
	{ key1 :: Int	
	} deriving(Eq, Show, Read)

data User = User
	{ username :: String
	, password :: String
	, permissions :: String
	} deriving (Generic, FromBSON, ToBSON, FromJSON, ToJSON)

data ResponseData = ResponseData
	{ responseData :: String }
	deriving(Generic)

instance FromJSON ResponseData
instance ToJSON ResponseData

deriving instance ToBSON String
deriving instance FromBSON String



type API = "insertUser" :> ReqBody '[JSON] User :> Post '[JSON] ResponseData
		:<|> "returnToken" :> ReqBody '[JSON] User :> Post '[JSON] ResponseData
		:<|> "getUserByName" :> QueryParam "search" String :> Get '[JSON] [User] 
		
		-- :<|> "generateToken" :> ReqBody '[JSON] User :> Post '[JSON] ResponseData
		
		-- :<|> "findUser" :> ReqBody '[JSON] User :> Post '[JSON] ResponseData



main = do 
	--ASCII Caesar Cipher
	
	handle <- openFile "text.txt" ReadMode
	textFile <- hGetContents handle
	--putStr textFile

	generatedKeyForEncryption <- generateKey
	let temp = encrypt textFile generatedKeyForEncryption
	print(temp)

encrypt :: String -> Int -> String
encrypt textToEncrypt encryptionKey = do
	let encryptInt = map ord textToEncrypt
	let applyKey = map (+encryptionKey) encryptInt
	let encryptedMessage = map chr applyKey
	return encryptedMessage!!0

decrypt :: String -> Int -> String
decrypt textToDecrypt decryptionKey = do
	let decryptInt = map ord textToDecrypt
	let applyKey = map (+(-decryptionKey)) decryptInt
	let decryptedMessage = map chr applyKey
	return decryptedMessage!!0


generateKey :: IO Int 
generateKey = randomRIO(1,25)


startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = insertUser
	:<|> returnToken
	:<|> getUserByName
	-- :<|> generateToken
	-- :<|> findUser


startMongoDB functionToRun = do
	pipe <- connect (host "127.0.0.1")
	e <- access pipe master "LFSdatabase" functionToRun
	print e
	close pipe

returnMongo :: Action IO a0 -> IO a0
returnMongo functionToRun = do
    pipe <- connect (host "127.0.0.1")
    e <- access pipe master "LFSdatabase" functionToRun
    close pipe
    return e

localKey :: Key
localKey = Key 7

token1 :: Token
token1 = Token localKey "READ WRITE"

returnToken :: User -> Handler ResponseData
returnToken userData = liftIO $ do
	let nameToFind = username userData
	let temp = password userData
	let passToFind = encrypt temp (key1 localKey)
	x <- startMongoDB $ findOne $ select ["username" =: nameToFind, "password" =: passToFind] "Users"
	return $ ResponseData (metadata token1)

-- generateToken :: User -> Handler ResponseData
-- generateToken userData = liftIO $ do
-- 	let nameToFind = username userData
-- 	let temp = password userData
-- 	let passToFind = encrypt temp (key1 localKey)
-- 	p <- startMongoDB $ find (select ["username" =: nameToFind, "password" =: passToFind] "Users") >>= rest
-- 	let tempToken = Token localKey p
-- 	return $ ResponseData (metadata tempToken)

generateToken :: User -> Handler ResponseData
generateToken userData = liftIO $ do
	let nameToFind = username userData
	let temp = password userData
	let passToFind = encrypt temp (key1 localKey)
	let userDoc = getUserByName nameToFind

	print(userDoc)
--	p <- startMongoDB $ findOne (select ["username" =: nameToFind, "password" =: passToFind] "Users") 
--	let tempToken = Token localKey p
	return $ ResponseData (username userData)


getUserByName :: Maybe String -> Handler [User]
getUserByName uname = liftIO $ do
	xyz <- returnMongo $ find (select ["username" =: uname] "Users") >>= rest
	return $ mapMaybe (\ b -> fromBSON b :: Maybe User) xyz

-- generateToken :: User -> Handler ResponseData
-- generateToken userData = liftIO $ do
-- 	let nameToFind username userData
-- 	let temp = password userData
-- 	let passToFind = encrypt temp (key1 localKey)
-- 	z <- startMongoDB $ find (select ["username" =: nameToFind] "Users") >>= rest
-- 	let tempToken = Token localKey z
-- 	return $ ResponseData (metadata tempToken)

-- findUser :: User -> Handler ResponseData
-- findUser userData = liftIO $ do
-- 	let nameToFind = username userData
-- 	let temp = password userData
-- 	let passToFind = encrypt temp (key1 localKey)
-- 	x <- startMongoDB $ findOne $ select ["username" =: nameToFind, "password" =: passToFind] "Users"
-- 	return $ ResponseData (username userData)

insertUserToDatabase :: Document -> IO()
insertUserToDatabase userToInsert = startMongoDB $ insert "Users" userToInsert

insertUser :: User -> Handler ResponseData
insertUser userData = liftIO $ do
	--encryptionKey <- generateKey
	let pass = password userData
	let name = username userData
	let encryptedPass = encrypt pass (key1 localKey)
	print(encryptedPass)
	let perm = permissions userData
	let tempUser = User name encryptedPass perm

	x <- insertUserToDatabase $ (toBSON $ tempUser)
	return $ ResponseData (username tempUser)


-- localKey :: Key
-- localKey = do
-- 	localKeyVariable <- generateKey
-- 	return $ Key localKeyVariable


-- Haskell hard to maintain state so would have to use an additional database in order to ensure the correct key was used for decryption



-- users :: [User]
-- users = [ User 1 "Isaac" "Newton"
--         , User 2 "Albert" "Einstein"
--         ]
