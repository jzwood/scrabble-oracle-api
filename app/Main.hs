import System.Environment (lookupEnv, getEnv)
import Data.Maybe (fromMaybe)
import Data.Either
import PostgresQueries

import App (app)

getPort :: IO Int
getPort = read . fromMaybe "3000" <$> lookupEnv "PORT" -- TODO: make 3000 if PORT is not an integer

pgEnv = PostgresEnv { pgHost = "localhost"
                    , pgPort = "1234"
                    , pgUser = "oracle"
                    , pgPassword = "mD361nKg7EwbfJCqeSoP9woNzAEjV"
                    , pgDBName = "scrabble-oracle-db"
                    }

smartLookup :: String -> IO (Either String String)
smartLookup envVar = do
  mEnv <- lookupEnv envVar
  case mEnv of
    Nothing -> return $ Left $ envVar ++ " not found"
    Just val -> return $ Right val

getPgEnv :: IO (Either String PostgresEnv)
getPgEnv = do
  db <- smartLookup "TPG_DB"
  user <- smartLookup "TPG_USER"
  password <- smartLookup "TPG_PASS"
  host <- smartLookup "TPG_HOST"
  port <- smartLookup "TPG_PORT"
  case (db, user, password, host, port) of
    (Right db', Right user', Right password', Right host', Right port') ->
      return $ Right $ PostgresEnv { pgHost = host'
                                   , pgPort = port'
                                   , pgUser = user'
                                   , pgPassword = password'
                                   , pgDBName = db'
                                   }
    _ -> return $ Left $ unlines $ lefts [db, user, password, host, port]

main :: IO ()
main = do
  port <- getPort
  ePgEnv <- getPgEnv
  case ePgEnv of
    Left err -> error err
    Right pgEng -> app port pgEnv
