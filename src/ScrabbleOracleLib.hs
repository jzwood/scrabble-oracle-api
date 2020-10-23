{-# LANGUAGE OverloadedStrings #-}

module ScrabbleOracleLib where

import Control.Applicative
import Control.Lens
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Time.Clock.POSIX
import GHC.Generics
import Network.Wreq
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

sendEmail :: String -> String -> IO (Response BL.ByteString)
sendEmail to html = postWith opts url [ "from" := ("Scrabble Oracle App <mailgun@sandboxc9f2eaeb1dc741a6b5e0849924393f85.mailgun.org>" :: String)
                                      , "to" := to
                                      , "subject" := ("Scrabble Oracle Best Play" :: String)
                                      , "html":= html
                                      , "require_tls" := ("true" :: String)
                                      ]
  where
    opts = defaults & auth ?~ basicAuth "api" "d9535fa5255cc199c13e54d16549239d-53c13666-3f24bd99"
    url = "https://api.mailgun.net/v3/sandboxc9f2eaeb1dc741a6b5e0849924393f85.mailgun.org/messages"

-- saves output to disc when mail gun api ENV is not set
emailBestPlay :: String -> String -> IO ()
emailBestPlay strBoard strRack =
  do
    (b, w, s) <- getBestPlay strBoard strRack
    let board = stringifyBoard b
        word = w
        score = show s
    --writeToFile $ unlines [stringifyBoard b, w, show s]
    sendEmail "jzwood14@gmail.com" $ unlines [board, word, score]
    return ()
