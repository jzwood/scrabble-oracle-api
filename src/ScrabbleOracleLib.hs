{-# LANGUAGE OverloadedStrings #-}

module ScrabbleOracleLib where

import Control.Applicative
import Data.Aeson
import Data.Time.Clock.POSIX
import GHC.Generics
import Web.Scotty

import Prelude
import qualified Prelude as P

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

writeToFile :: String -> IO ()
writeToFile str = do
  time <- show . round <$> getPOSIXTime
  P.writeFile ("./media/board_" ++ time ++ ".txt") str
  return ()

-- saves output to disc when mail gun api ENV is not set
emailBestPlay :: String -> String -> IO ()
emailBestPlay strBoard strRack =
  do
    (b, w, s) <- getBestPlay strBoard strRack
    writeToFile $ unlines [stringifyBoard b, w, show s]
    return ()
