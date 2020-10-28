{-# LANGUAGE OverloadedStrings #-}

module ScrabbleOracleLib where

import Control.Applicative
import Data.Aeson
import Data.Time.Clock.POSIX
import GHC.Generics
import Web.Scotty
import System.Process

import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.ByteString.UTF8 as BSU      -- from utf8-string

import Prelude
import qualified Prelude as P

import Game.SingleBestPlay
import Game.ScrabbleBoard
import EmailTemplate


import Data.Maybe (fromMaybe)

validateAddress :: String -> Bool
validateAddress = any ((=='@') .|| (=='.')) -- silly I know but it's good enough ftm

validateInput :: String -> String -> String -> Bool
validateInput board rack address =
  parseBoard board /= Nothing &&
    parseRack rack /= Nothing &&
      validateAddress address

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
sendEmail to from html = createProcess (shell curl) >>= cleanupProcess
  where
    body = unlines [ "Subject: Scrabble Oracle Best Play"
                   , "From:", from
                   , "Content-Type: text/html; charset=\"utf8\""
                   , html
                   ]
    curl = unwords ["curl -sS --url 'smtps://smtp.gmail.com:465' --ssl-reqd --mail-from" , from
                   , "--mail-rcpt" , to
                   , "--upload-file <(echo " ++ body ++ "\")"
                   , "--user \"" , from ++ ":$EMAIL_APP_KEY\""  -- curl can access env variables
                   ]

-- saves output to disc when either to or from address is Nothing
emailBestPlay :: Maybe String -> Maybe String -> String -> String -> IO ()
emailBestPlay maybeFrom maybeTo strBoard strRack = do
    (board, word, score) <- getBestPlay strBoard strRack
    let emailHTML = emailTemplate board word score
    case (maybeFrom, maybeTo) of
      (Just from, Just to) -> sendEmail to from emailHTML
      _ -> writeToFile emailHTML
