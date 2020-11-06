{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Control.Concurrent
import Control.Applicative
import Data.Char as Char
import GHC.Generics
import Network.HTTP.Types
import System.Environment (lookupEnv, getEnv)
import Data.Maybe (fromMaybe)
import Data.Text as TS
import Data.Text.Lazy as TL
import Prelude

import ScrabbleOracleLib

import Web.Scotty
import Data.Aeson hiding (json)

import Game.ScrabbleBoard

up :: String -> String
up = Prelude.map Char.toUpper

data ScrabbleOraclePost = PostJson
  { board :: String
  , rack :: String
  , rcpt :: String
  } deriving Generic


instance ToJSON ScrabbleOraclePost where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ScrabbleOraclePost

getPort :: IO Int
getPort = read . fromMaybe "3000" <$> lookupEnv "PORT" -- TODO: make 3000 if PORT is not an integer

main = do
    port <- getPort
    scotty port $ do
      options "/ask-the-scrabble-oracle" $
        do
          status status204
          setHeader "Access-Control-Allow-Methods" "POST"
          setHeader "Access-Control-Allow-Headers" "Origin, Content-Type, Accept"
          setHeader "Access-Control-Allow-Origin" "http://localhost:1234"
      post "/ask-the-scrabble-oracle" $
        do
          jsonReq <- jsonData :: ActionM ScrabbleOraclePost
          setHeader "Access-Control-Allow-Headers" "Origin, Content-Type, Accept"
          setHeader "Access-Control-Allow-Origin" "http://localhost:1234"
          let strBoard = board jsonReq
              strRack = rack jsonReq
              strAddress = rcpt jsonReq
              validInput = validateInput strBoard strRack strAddress
          case validInput of
            Left errors -> do
              status badRequest400
              text $ TL.pack errors
            Right _ -> do
              liftAndCatchIO $ forkIO $ emailBestPlay strAddress strBoard strRack
              status status202
              text "task scheduled"
      notFound $ text "there is no such route."
