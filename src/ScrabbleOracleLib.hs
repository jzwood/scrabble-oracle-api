{-# LANGUAGE OverloadedStrings #-}

module ScrabbleOracleLib where

import Control.Applicative
import Data.Aeson
import Data.Time.Clock.POSIX
import GHC.Generics
import System.Environment (lookupEnv, getEnv)
import System.Process
import Web.Scotty
import Data.UUID.V4 (nextRandom)

import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.ByteString.UTF8 as BSU      -- from utf8-string

import Prelude
import qualified Prelude as P

import Game.SingleBestPlay
import Game.ScrabbleBoard
import EmailTemplate


import Data.Maybe (fromMaybe, isJust)

validateAddress :: String -> Bool
validateAddress = elem '@' .&&  elem '.'

validateInput :: String -> String -> String -> Either String Bool
validateInput board rack address = boardIsValid >>= rackIsValid >>= addressIsValid
  where
    boardIsValid = if isJust (parseBoard board) then Right True else Left "malformed board"
    rackIsValid _ = if isJust (parseRack rack) then Right True else Left "malformed rack"
    addressIsValid _ = if validateAddress address then Right True else Left "malformed rcpt email address"

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
  P.writeFile ("./media/board_" ++ time ++ ".html") str
  return ()

sendEmail :: String -> String -> String -> IO ()
sendEmail from to html = do
  uuid <- nextRandom
  let emailFilePath = "mail-" ++ show uuid ++ ".txt"
  P.writeFile emailFilePath body
  curlProc@(_, _, _, processHandle) <- createProcess (shell $ curl emailFilePath)
  waitForProcess processHandle
  cleanupProcess curlProc
  where
    body = unlines [ "From: Scrabble Oracle App <" ++ from ++ ">"
                   , "To: " ++ to
                   , "Subject: Scrabble Oracle Best Play"
                   , "Content-Type: text/html; charset=\"utf8\"\n"
                   , html
                   ]
    curl :: String -> String
    curl emailFilePath = unwords [ "curl -sS --url 'smtps://smtp.gmail.com:465' --ssl-reqd"
                                 , "--mail-from $MAIL_FROM"
                                 , "--mail-rcpt" , to
                                 , "--upload-file", emailFilePath
                                 , "--user \"$MAIL_FROM:$MAIL_APP_KEY\""  -- curl can access env variables
                                 ]

isProduction :: IO Bool
isProduction = (==Just "TRUE") <$> lookupEnv "PRODUCTION"

fromAddress :: IO (Maybe String)
fromAddress = lookupEnv "MAIL_FROM"

-- saves output to disc when either to or from address is Nothing
emailBestPlay :: String -> String -> String -> IO ()
emailBestPlay strAddress strBoard strRack = do
    (board, word, score) <- getBestPlay strBoard strRack
    production <- isProduction
    let emailHTML = emailTemplate board word score
    if production
      then do
        maybeFrom <- fromAddress
        case maybeFrom of
          Nothing -> putStrLn "missing from address"
          Just from -> do
            putStrLn $ "sending email to " ++ strAddress
            sendEmail from strAddress emailHTML
      else do
        putStrLn "writing email to disc"
        writeToFile emailHTML
