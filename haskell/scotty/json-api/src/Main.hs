{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types
import Data.HashMap.Strict (fromList)
import Data.Monoid ((<>))
import GHC.Generics
import Web.Scotty

data Candidate = Candidate 
  { number :: Int
  , name :: String 
  , votes :: Float
  } deriving (Show, Generic)

instance ToJSON Candidate
instance FromJSON Candidate


candidates =
  [ Candidate 12 "Ciro" 0.25
  , Candidate 13 "Haddad" 0.25
  , Candidate 17 "Bolsonaro" 0.25
  , Candidate 45 "Alckmin" 0.25
  ]

urls = fromList
  [ ("Say hello", String "/hello")
  , ("List of candidates", String "/candidates")
  ]


matchesId :: Int -> Candidate -> Bool
matchesId id user = number user == id

main = do
  putStrLn "Starting Server..."
  scotty 3000 $ do
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