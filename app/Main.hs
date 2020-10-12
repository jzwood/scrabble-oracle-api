{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics

import Control.Applicative
import Data.Char (toUpper)
import Data.Maybe (fromMaybe)
import Web.Scotty
import Data.Aeson
import Network.HTTP.Types
import Game.SingleBestPlay
import Game.ScrabbleBoard
import qualified Data.Text.Lazy as L


up :: String -> String
up = map toUpper


data OracleAPIOutput = OracleAPIOutput { board :: String
                                          , word :: String
                                          , score :: Score
                                          } deriving (Generic, Show)

instance ToJSON OracleAPIOutput where
    toJSON (OracleAPIOutput board word score) =
      object ["board" .= board, "word" .= word, "score" .= score]

main = scotty 3000 $ do
  get (regex "^/board/([a-zA-Z1234_]{225})/rack/([a-zA-Z]{5,7})$") $
    do
      strBoard <- up . L.unpack <$> param "1"
      strRack <- up . L.unpack <$> param "2"
      let board = parseBoard strBoard
          rack = parseRack strRack
          maybeIOBestPlay = liftA2 makeSinglePlay board rack
          bestPlay = fromMaybe (return (nullBoard, "", 0)) maybeIOBestPlay
      (b, w, s) <- liftAndCatchIO bestPlay
      Web.Scotty.json $ OracleAPIOutput (stringifyBoard b) w s
  notFound $ text "there is no such route."
