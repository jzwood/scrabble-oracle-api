{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Applicative
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Network.HTTP.Types
import System.Environment (lookupEnv, getEnv)
import qualified Data.Text.Lazy as L

import ScrabbleOracleLib

import Web.Scotty
import Data.Aeson

import Game.ScrabbleBoard

up :: String -> String
up = map toUpper

getPort :: IO Int
getPort = (\s -> read s :: Int) <$> getEnv "PORT"

main = do
    port <- getPort  -- this will fail if $PORT is undefined or not an int. todo refactor with Maybe.Read and lookupEnv
    scotty port $ do
      get (regex "^/board/([a-zA-Z1234_]{225})/rack/([a-zA-Z]{7})$") $
        do
          strBoard <- up . L.unpack <$> param "1"
          strRack <- up . L.unpack <$> param "2"
          liftAndCatchIO $ forkIO $ emailBestPlay strBoard strRack
          Web.Scotty.text "task scheduled"
      notFound $ text "there is no such route."
