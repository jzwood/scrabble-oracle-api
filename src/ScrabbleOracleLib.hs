{-# LANGUAGE OverloadedStrings #-}

module ScrabbleOracleLib where

import Control.Applicative
import Control.Lens
import Data.Aeson
import Data.Time.Clock.POSIX
import GHC.Generics
import Network.Wreq
import Web.Scotty

import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
import Data.ByteString.UTF8 as BSU      -- from utf8-string

import Prelude
import qualified Prelude as P

import Game.SingleBestPlay
import Game.ScrabbleBoard
import EmailTemplate

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
  P.writeFile ("./media/board_" ++ time ++ ".html") str
  return ()

sendEmail :: BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> IO (Response BLU.ByteString)
sendEmail apiKey to html = postWith opts url
  [ "from" := ("Scrabble Oracle App <mailgun@sandboxc9f2eaeb1dc741a6b5e0849924393f85.mailgun.org>" :: BSU.ByteString)
  , "to" := to
  , "subject" := ("Scrabble Oracle Best Play" :: BSU.ByteString)
  , "html":= html
  , "require_tls" := ("true" :: BSU.ByteString)
  ]
  where
    opts = defaults & auth ?~ basicAuth "api" apiKey
    url = "https://api.mailgun.net/v3/sandboxc9f2eaeb1dc741a6b5e0849924393f85.mailgun.org/messages"

-- saves output to disc when mail gun api ENV is not set
emailBestPlay :: Maybe String -> String -> String -> IO ()
emailBestPlay maybeApiKey strBoard strRack = do
    (board, word, score) <- getBestPlay strBoard strRack
    let emailHTML = emailTemplate board word score
    case maybeApiKey of
      Nothing -> writeToFile emailHTML
      Just apiKey -> do
        sendEmail (BSU.fromString apiKey) "jzwood14@gmail.com" $ BSU.fromString emailHTML
        return ()
