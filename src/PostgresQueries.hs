{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PostgresQueries where

import Database.PostgreSQL.Simple
import Data.Int
import Data.UUID.Types
import Data.ByteString.UTF8 as BSU
import Prelude hiding (head)

data PostgresEnv = PostgresEnv
  { pgHost :: String
  , pgPort :: String
  , pgUser :: String
  , pgPassword :: String
  , pgDBName :: String
  } deriving (Show)

ask :: PostgresEnv -> BSU.ByteString
ask PostgresEnv { pgHost = host
                , pgPort = port
                , pgUser = user
                , pgPassword = password
                , pgDBName = dbName
                } = BSU.fromString $ unwords [ "host='" ++ host ++ "'"
                                             , "port='" ++ port ++ "'"
                                             , "user='" ++ user ++ "'"
                                             , "password='" ++ password ++ "'"
                                             , "dbname='" ++ dbName ++ "'"
                                             ]

head :: [a] -> Maybe a
head [] = Nothing
head (x:xs) = Just x

-- USER SUBMITS BOARD AND RACK
putRackBoard :: PostgresEnv -> String -> String -> IO (Maybe (Integer, UUID))
putRackBoard pgEnv rack board = do
  conn <- connectPostgreSQL $ ask pgEnv
  ids :: [(Integer, UUID)] <- query conn "INSERT INTO oracle.query_rack_board (rack, board) \
    \ VALUES (?, ?) \
      \ ON CONFLICT DO NOTHING \
        \ RETURNING id, uuid;" (rack, board)
  return $ head ids

getUUIDByRackBoard :: PostgresEnv -> String -> String -> IO (Maybe UUID)
getUUIDByRackBoard pgEnv rack board = do
  conn <- connectPostgreSQL $ ask pgEnv
  ids :: [(Integer, UUID)] <- query conn "SELECT id, uuid FROM oracle.query_rack_board \
    \ WHERE BOARD = ? AND rack = ?;" (board, rack)
  return $ head $ snd <$> ids

-- ALTERNATE: AFTER SYSTEM CALCULATES BEST PLAY INSERTS IT INTO DB
putBestPlay :: PostgresEnv -> String -> String -> Integer -> Integer -> IO Bool
putBestPlay pgEnv board word score fk = do
  conn <- connectPostgreSQL $ ask pgEnv
  num <- execute conn "INSERT INTO oracle.best_play (board, word, score, query_rack_board_id) \
                      \ VALUES (?, ?, ?, ?);" (board, word, score, fk)
  return $ num /= 0

doesBoardRackUuidExist :: PostgresEnv -> String -> IO Bool
doesBoardRackUuidExist pgEnv uuid = do
  conn <- connectPostgreSQL $ ask pgEnv
  [Only exists] <- query conn "SELECT EXISTS(SELECT id FROM oracle.query_rack_board WHERE uuid = ?);" [uuid]
  return exists

-- USER HAS UUID AND WOULD LIKE TO SEE IF THERE IS A CORRESPONDING BEST PLAY
getBestPlayByUUID :: PostgresEnv -> String -> IO (Maybe (String, Int64, String))
getBestPlayByUUID pgEnv uuid = do
  conn <- connectPostgreSQL $ ask pgEnv
  res :: [(String, Int64, String)] <- query conn "SELECT bp.word, bp.score, bp.board FROM oracle.best_play AS bp \
                                                  \ INNER JOIN oracle.query_rack_board AS qrb ON qrb.id = bp.query_rack_board_id \
                                                  \ WHERE qrb.uuid = ?;" [uuid]  -- there should only every be one
  return $ head res
