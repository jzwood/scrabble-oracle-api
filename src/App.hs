{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module App where

import Control.Concurrent
import Control.Applicative
import Data.Char as Char
import GHC.Generics
import Network.HTTP.Types
import System.Environment (lookupEnv, getEnv)
import Data.Maybe (fromMaybe)
import Data.Text as TS
import Data.Text.Lazy as TL

import ScrabbleOracleLib

import Web.Scotty
import Data.Aeson hiding (json)

import Game.ScrabbleBoard
import Utils (up)


data ScrabbleOraclePost = PostJson
  { board :: String
  , rack :: String
  , rcpt :: String
  } deriving Generic


instance ToJSON ScrabbleOraclePost where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ScrabbleOraclePost

tellOracleRoute = "/tell-the-scrabble-oracle" -- post board/rack data
askOracleRoute = "^/ask-the-scrabble-oracle/([0-9a-fA-F]{8}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{4}\b-[0-9a-fA-F]{12}$)" -- get word/score

getBestPlayByUUID :: TL.Text -> Integer
getBestPlayByUUID _ = 200 -- TODO

addNewBoardRackQuery :: String -> String -> IO ()
addNewBoardRackQuery board rack = undefined

app :: Int -> IO ()
app port =
    scotty port $ do
      options tellOracleRoute $
        do
          status status204
          setHeader "Access-Control-Allow-Methods" "POST"
          setHeader "Access-Control-Allow-Headers" "Origin, Content-Type, Accept"
          setHeader "Access-Control-Allow-Origin" "*" -- change to something more specific once I've deployed UI
      options (regex askOracleRoute) $
        do
          status status204
          setHeader "Access-Control-Allow-Methods" "GET"
          setHeader "Access-Control-Allow-Headers" "Origin, Content-Type, Accept"
          setHeader "Access-Control-Allow-Origin" "*"
      post tellOracleRoute $
        do
          jsonReq <- jsonData :: ActionM ScrabbleOraclePost
          setHeader "Access-Control-Allow-Headers" "Origin, Content-Type, Accept"
          setHeader "Access-Control-Allow-Origin" "*" -- change to something more specific once I've deployed UI
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
      get (regex askOracleRoute) $
        do
          uuid <- param "1"
          setHeader "Access-Control-Allow-Headers" "Origin, Content-Type, Accept"
          setHeader "Access-Control-Allow-Origin" "*"
          status status200
          text uuid
      notFound $ text "there is no such route."
