{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module App where

import Control.Applicative
import Control.Concurrent
import Data.Aeson hiding (json)
import Data.ByteString.UTF8 as BSU
import Data.Char as Char
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Text.Lazy as TL
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

data BoardRackJson = BoardRackJson
  { board :: String
  , rack :: String
  } deriving Generic

instance ToJSON BoardRackJson where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BoardRackJson

data BestPlayJson = BestPlayJson
  { newBoard :: String
  , word :: String
  , score :: Int64
  } deriving Generic

instance ToJSON BestPlayJson where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BestPlayJson

tellOracleRoute = "/tell-the-scrabble-oracle"
askOracleRoute = "^/ask-the-scrabble-oracle/([0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$)" -- get word/score

app :: Int -> PostgresEnv -> IO ()
app port pgEnv =
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
          jsonReq <- jsonData :: ActionM BoardRackJson
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
              maybeIdUUID <- liftAndCatchIO $ putRackBoard pgEnv strRack strBoard
              host <- fromMaybe "unknownhost" . requestHeaderHost <$> request
              let domain = BSU.toString host ++ "/ask-the-scrabble-oracle/"  -- ? prepend with https://
              case maybeIdUUID of
                Nothing -> do -- rack and board already exist in DB
                  muuid <- liftAndCatchIO $ getUUIDByRackBoard pgEnv strRack strBoard
                  let uuid = maybe "" UUID.toString muuid
                  status status200
                  text $ TL.pack $ domain ++ uuid
                Just (id, uuid) -> do
                  liftAndCatchIO $ forkIO $ saveBestPlay pgEnv parsedBoard parsedRack id
                  status status202
                  text $ TL.pack $ domain ++ UUID.toString uuid
      get (regex askOracleRoute) $
        do
          uuid <- param "1"
          setHeader "Access-Control-Allow-Headers" "Origin, Content-Type, Accept"
          setHeader "Access-Control-Allow-Origin" "*"
          boardRackExists <- liftAndCatchIO $ doesBoardRackUuidExist pgEnv uuid
          if not boardRackExists then do
              status status404
              text "resource not found"
          else do
            status status200
            mBestPlay <- liftAndCatchIO $ getBestPlayByUUID pgEnv uuid
            case mBestPlay of
              Nothing -> text "still processing..."
              Just (word, score, board) -> json $ BestPlayJson { newBoard = board, word = word, score = score }
      notFound $ text "there is no such route."
