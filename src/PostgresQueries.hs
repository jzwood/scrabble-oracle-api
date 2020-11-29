{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgresQueries where

import Database.PostgreSQL.Simple
import Data.Int
import Data.UUID.Types
import Prelude hiding (head)

head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = Just x

-- USER SUBMITS BOARD AND RACK
putRackBoard :: String -> String -> IO (Maybe (Integer, UUID))
putRackBoard rack board = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  ids :: [(Integer, UUID)] <- query conn "INSERT INTO oracle.query_rack_board (rack, board) \
    \ VALUES (?, ?) \
      \ ON CONFLICT DO NOTHING \
        \ RETURNING id, uuid;" (rack, board)
  return $ head ids

getUUIDByRackBoard :: String -> String -> IO (Maybe UUID)
getUUIDByRackBoard rack board = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  ids :: [(Integer, UUID)] <- query conn "SELECT id, uuid FROM oracle.query_rack_board \
    \ WHERE BOARD = ? AND rack = ?;" (board, rack)

  return $ head $ snd <$> ids

-- AFTER SYSTEM CALCULATES BEST PLAY INSERTS IT INTO DB
putBestPlay :: String -> String -> String -> String -> Integer -> IO Bool
putBestPlay oRack oBoard nBoard word score = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  num <- execute conn "INSERT INTO oracle.best_play (board, word, score, query_rack_board_id) \
                      \ SELECT ?, ?, ?, qrb.id FROM oracle.query_rack_board AS qrb \
                      \ WHERE qrb.rack = ? AND qrb.board = ?;" (nBoard, word, score, oRack, oBoard)
  return $ num /= 0

-- ALTERNATE: AFTER SYSTEM CALCULATES BEST PLAY INSERTS IT INTO DB
putBestPlay' :: String -> String -> Integer -> Integer -> IO Bool
putBestPlay' board word score fk = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  num <- execute conn "INSERT INTO oracle.best_play (board, word, score, query_rack_board_id) \
                      \ VALUES (?, ?, ?, ?);" (board, word, score, fk)
  return $ num /= 0

-- USER HAS UUID AND WOULD LIKE TO SEE IF THERE IS A CORRESPONDING BEST PLAY
getBestPlayByUUID :: String -> IO (Maybe (String, Int64, String))
getBestPlayByUUID uuid = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  res :: [(String, Int64, String)] <- query conn "SELECT bp.word, bp.score, bp.board FROM oracle.best_play AS bp INNER JOIN oracle.query_rack_board AS qrb ON qrb.id = bp.query_rack_board_id WHERE qrb.uuid = ?;" [uuid]  -- there should only every be one
  return $ head res


main :: IO ()
main = do
  --c <- putRackBoard "ASZFHUI" "QWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPA"
  --print c
  a <- getUUIDByRackBoard "ASZFHUI" "QWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPA"
  print a
  --d <- getBestPlayByUUID "cfb066af-b454-424c-99f7-36b92212c91c"
  --print d
  --e <- putBestPlay "ASDFEUI" "QWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPA" "HDSHFI" "IJDIHUFHYK" 99
  --print e
