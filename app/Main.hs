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
getPort = read . fromMaybe "3000" <$> lookupEnv "PORT"

getFrom :: IO (Maybe String)
getFrom = lookupEnv "MAIL_FROM"

main = do
    port <- getPort
    address <- getFrom
    scotty port $ do
      post "/ask-the-scrabble-oracle" $
        do
          jsonReq <- jsonData :: ActionM ScrabbleOraclePost
          setHeader "Content-Type" "application/text"
          let strBoard = board jsonReq
              strRack = rack jsonReq
              strAddress = rcpt jsonReq
              validInput = validateInput strBoard strRack strAddress
          case validInput of
            Left errors -> do
              status badRequest400
              text $ TL.pack errors
            Right _ -> do
              status status202
              text "task scheduled"
      notFound $ text "there is no such route."



--liftAndCatchIO $ forkIO $ emailBestPlay maybeApiKey strBoard strRack
--(regex "^/board/([a-zA-Z1234_]{225})/rack/([a-zA-Z]{7})$") $
