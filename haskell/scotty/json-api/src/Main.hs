{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types
import qualified Data.Char as C
import qualified Data.Text as T
import Data.HashMap.Strict (fromList)
import Data.Monoid ((<>))
import GHC.Generics
import Web.Scotty

instance ToJSON Candidate
instance FromJSON Candidate
data Candidate = Candidate 
  { number :: Int
  , name :: String 
  , votes :: Float
  } deriving (Show, Generic)


candidates =
  [ Candidate 12 "Ciro" 0.25
  , Candidate 13 "Haddad" 0.25
  , Candidate 17 "Bolsonaro" 0.25
  , Candidate 45 "Alckmin" 0.25
  , Candidate 51 "Cabo Daciolo" 0.0
  ]

urls = fromList
  [ ("Say hello", String "/hello")
  , ("List of candidates", String "/candidates")
  ]


matchesId :: Int -> Candidate -> Bool
matchesId id user = number user == id

main = do
  putStrLn "Starting Server..."
  scotty 8000 $ do
  
    get "/" $ do
      json $ Object urls


    get "/hello" $ do
      text "hello world!"


    get "/hello/:name" $ do
      name <- param "name"
      text ("hello " <> name <> "!")


    get "/candidates" $ do
      json candidates


    get "/candidates/:id" $ do
      id <- param "id"
      json (filter (matchesId id) candidates)

    get "/number-of-the-beast" $ do
      json (666 :: Int)

    get "/my-number/:name" $ do
      name <- param "name"
      json (numberOf name)


numberOf :: String -> Int
numberOf "" = 0
numberOf (c:cs) = (num (clean c)) + numberOf cs
  where
    num c = mapNumber (C.ord c)
    clean = C.toLower
    mapNumber x = 
      if x <= 96 then
        0
      else if x <= 106 then
        x - 96
      else if x <= 115 then
        10 * (x - 105)
      else if x <= 122 then
        100 * (x - 114)
      else
        0