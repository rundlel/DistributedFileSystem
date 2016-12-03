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
	let s = "sample string"
	let x = map ord s
	--print (s)
	--print (x)
	let z = map (+4) x
	--print (z)
	let y = map chr z
	--print (y)
	let a = map ord y
	let b = map (+(-4)) a
	--print (b)
	let c = map chr b
	--print (c)

	--verify username/password with database
	--give them an encrypted token
	--token has metadata and a key
	--metadata encrypted with key1
	--key1 encrypted with key2

	handle <- openFile "text.txt" ReadMode
	textFile <- hGetContents handle
	--putStr textFile

	something <- generateKey
	let temp = encrypt textFile something
	print(temp)

	let fileToInt = map ord textFile
	--print(fileToInt)

	let encryptFile = map (+something) fileToInt
	--print (encryptFile) 

	let encryptedText = map chr encryptFile
	--putStrLn(encryptedText)

	-- let encryptedTextToInt = map ord encryptedText


	let decryptText = map (+(-(something))) encryptFile
	--print(decryptText)

	let decryptedMessage = map chr decryptText
	--putStrLn(decryptedMessage)


	
	print(something)

encrypt :: String -> Int -> [String]
encrypt textToEncrypt param = do
	let encryptInt = map ord textToEncrypt
	let applyKey = map (+param) encryptInt
	let encryptedMessage = map chr applyKey
	return encryptedMessage


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
