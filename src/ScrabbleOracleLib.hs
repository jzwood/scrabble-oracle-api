module ScrabbleOracleLib where

import Control.Applicative

import Game.SingleBestPlay
import Game.ScrabbleBoard

import Data.Maybe (fromMaybe)


getBestPlay :: String -> String -> IO (Board, String, Score)
getBestPlay strBoard strRack = bestPlay
  where
    board = parseBoard strBoard
    rack = parseRack strRack
    maybeIOBestPlay = liftA2 makeSinglePlay board rack
    bestPlay = fromMaybe (return (nullBoard, "", 0)) maybeIOBestPlay
