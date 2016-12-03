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
	print (s)
	print (x)
	let z = map (+4) x
	print (z)
	let y = map chr z
	print (y)
	let a = map ord y
	let b = map (+(-4)) a
	print (b)
	let c = map chr b
	print (c)


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
