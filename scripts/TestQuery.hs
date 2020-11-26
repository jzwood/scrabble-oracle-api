{-# LANGUAGE OverloadedStrings #-}


import Database.PostgreSQL.Simple
import Data.Int

grack :: IO String
grack = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  [Only i] <- query_ conn "SELECT rack FROM oracle.query_rack_board LIMIT 1;"
  return i

pbestplay :: IO Int64
pbestplay = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  num <- execute_ conn "INSERT INTO oracle.best_play (board, word, score, query_rack_board_id) VALUES ('JUIHDUFHYK', 'HUMMM', 99, 1);"
  return num


pboardrack :: IO Int64
pboardrack = do
  conn <- connectPostgreSQL "host='localhost' port='1234' user='oracle' password='mD361nKg7EwbfJCqeSoP9woNzAEjV' dbname='scrabble-oracle-db'"
  num <- execute_ conn "INSERT INTO oracle.query_rack_board (rack, board) VALUES ('ASDFEUI', 'QWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERQWERTYUIOPQWERTYUIOPQWERTYUIOPQWERTYUIOPA') ON CONFLICT DO NOTHING;"
  return num

main :: IO ()
main = do
  a <- grack
  print a
  b <- pbestplay
  print b
  c <- pboardrack
  print c

