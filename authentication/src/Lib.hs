{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
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


data Token = Token
	{ key :: Key
	, metadata :: String
	} deriving (Show, Read)

data Key = Key
	{ key1 :: Int	
	} deriving(Eq, Show, Read)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]


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
server = return users

users :: [User]
users = [ User 1 "Isaac" "Newton"
        , User 2 "Albert" "Einstein"
        ]
