{-# LANGUAGE OverloadedStrings #-}

module PostgresQueries where

import Database.PostgreSQL.Simple
import Data.Int
import Prelude hiding (head)


head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = Just x

-- USER SUBMITS BOARD AND RACK
putRackBoard :: String -> String -> IO Int64
putRackBoard rack board = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  execute conn "INSERT INTO oracle.query_rack_board (rack, board) \
                \ VALUES (?, ?) \
                \ ON CONFLICT DO NOTHING;" (rack, board)

-- AFTER SYSTEM CALCULATES BEST PLAY INSERTS IT INTO DB
putBestPlay :: String -> String -> String -> String -> Integer -> IO Int64
putBestPlay oRack oBoard nBoard word score = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  execute conn "INSERT INTO oracle.best_play (board, word, score, query_rack_board_id) \
                \ SELECT ?, ?, ?, qrb.id FROM oracle.query_rack_board AS qrb \
                \ WHERE qrb.rack = ? AND qrb.board = ?;" (nBoard, word, score, oRack, oBoard)

-- ALTERNATE: AFTER SYSTEM CALCULATES BEST PLAY INSERTS IT INTO DB
putBestPlay' :: String -> String -> Integer -> String -> IO Int64
putBestPlay' board word score fk = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  execute conn "INSERT INTO oracle.best_play (board, word, score, query_rack_board_id) \
                \ VALUES (?, ?, ?, ?);" (board, word, score, fk)

-- USER HAS UUID AND WOULD LIKE TO SEE IF THERE IS A CORRESPONDING BEST PLAY
getBestPlayByUUID :: String -> IO (Maybe (String, Int64, String))
getBestPlayByUUID uuid = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  res <- query conn "SELECT bp.word, bp.score, bp.board FROM oracle.best_play AS bp INNER JOIN oracle.query_rack_board AS qrb ON qrb.id = bp.query_rack_board_id WHERE qrb.uuid = ?;" [uuid]  -- there should only every be one
  return $ head res


main :: IO ()
main = do
  c <- putRackBoard "ASZFHUI" "QWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPA"
  print c
  d <- getBestPlayByUUID "8dcc55c4-c5e2-4e59-a3d8-8f7564349d26"
  print d
  e <- putBestPlay "ASDFEUI" "QWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPA" "HDSHFI" "IJDIHUFHYK" 99
  print e

