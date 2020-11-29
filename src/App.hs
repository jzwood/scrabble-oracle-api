{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import Control.Applicative
import Control.Concurrent
import Data.Aeson hiding (json)
import Data.Char as Char
import Data.Maybe (fromMaybe)
import Data.Text.Lazy as TL
import Data.ByteString.UTF8 as BSU
import Data.UUID.Types as UUID
import GHC.Generics
import Network.HTTP.Types
import Network.Wai (requestHeaderHost)
import System.Environment (lookupEnv, getEnv)
import Web.Scotty

import Game.ScrabbleBoard
import Game.SingleBestPlay
import PostgresQueries

import Utils


data ScrabbleOraclePost = PostJson
  { board :: String
  , rack :: String
  , rcpt :: String
  } deriving Generic


instance ToJSON ScrabbleOraclePost where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON ScrabbleOraclePost

tellOracleRoute = "/tell-the-scrabble-oracle"
askOracleRoute = "^/ask-the-scrabble-oracle/([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$)" -- get word/score


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
              mBoard = parseBoard strBoard
              mRack = parseRack strRack
          case (mBoard, mRack) of
            (Nothing, _) -> do
              status badRequest400
              text $ TL.pack "malformed board"
            (_, Nothing) -> do
              status badRequest400
              text $ TL.pack "malformed rack"
            (Just parsedBoard, Just parsedRack) -> do
              maybeIdUUID <- liftAndCatchIO $ putRackBoard strRack strBoard
              host <- fromMaybe "unknownhost" . requestHeaderHost <$> request
              let domain = "https://" ++ BSU.toString host ++ "/ask-the-scrabble-oracle/"
              case maybeIdUUID of
                Nothing -> do -- rack and board already exist in DB
                  muuid <- liftAndCatchIO $ getUUIDByRackBoard strRack strBoard
                  let uuid = maybe "" UUID.toString muuid
                  status status200
                  text $ TL.pack $ domain ++ uuid
                Just (id, uuid) -> do
                  liftAndCatchIO $ forkIO $ saveBestPlay parsedBoard parsedRack id
                  status status202
                  text $ TL.pack $ domain ++ UUID.toString uuid
      get (regex askOracleRoute) $
        do
          uuid <- param "1"
          setHeader "Access-Control-Allow-Headers" "Origin, Content-Type, Accept"
          setHeader "Access-Control-Allow-Origin" "*"
          status status200
          text uuid
      notFound $ text "there is no such route."
