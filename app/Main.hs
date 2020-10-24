{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Applicative
import Data.Char as Char
import GHC.Generics
import Network.HTTP.Types
import System.Environment (lookupEnv, getEnv)
import Data.Text as TS
import Data.Text.Lazy as TL
import Prelude

import ScrabbleOracleLib

import Web.Scotty
import Data.Aeson

import Game.ScrabbleBoard

up :: String -> String
up = Prelude.map Char.toUpper

getPort :: IO Int
getPort = (\s -> read s :: Int) <$> getEnv "PORT"

getMailGunAPIKey :: IO (Maybe String)
getMailGunAPIKey = lookupEnv "MAILGUN_API_KEY"

main = do
    port <- getPort  -- this will fail if $PORT is undefined or not an int. todo refactor with Maybe.Read and lookupEnv
    maybeApiKey <- getMailGunAPIKey
    scotty port $ do
      get (regex "^/board/([a-zA-Z1234_]{225})/rack/([a-zA-Z]{7})$") $
        do
          strBoard <- up . TL.unpack <$> param "1"
          strRack <- up . TL.unpack <$> param "2"
          liftAndCatchIO $ forkIO $ emailBestPlay maybeApiKey strBoard strRack
          Web.Scotty.text "task scheduled"
      notFound $ text "there is no such route."
