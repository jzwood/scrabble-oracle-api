module Utils where

import Control.Applicative
import Data.Char as Char
import Data.Maybe (fromMaybe, isJust)
import Data.Matrix
import PostgresQueries
import qualified Data.Matrix as Mat

import Game.SingleBestPlay
import Game.ScrabbleBoard


validateBoard :: String -> Either String String
validateBoard board =
  case parseBoard board of
    Nothing -> Left "malformed board"
    Just _ -> Right board

validateRack :: String -> Either String String
validateRack rack =
  case parseRack rack of
    Nothing -> Left "malformed rack"
    Just _ -> Right rack

getBestPlay :: String -> String -> IO (Board, String, Score)
getBestPlay strBoard strRack = bestPlay
  where
    board = parseBoard strBoard
    rack = parseRack strRack
    maybeIOBestPlay = liftA2 makeSinglePlay board rack
    bestPlay = fromMaybe (return (nullBoard, "", 0)) maybeIOBestPlay

saveBestPlay :: PostgresEnv -> Board -> Rack -> Integer -> IO ()
saveBestPlay pgEnv board rack fk = do
    (board', word, score) <- makeSinglePlay board rack
    putBestPlay pgEnv (stringifyBoard board') word score fk
    return ()

up :: String -> String
up = map Char.toUpper
