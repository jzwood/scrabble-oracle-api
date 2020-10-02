{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.HTTP.Types

main = scotty 3000 $ do
  get (regex "^/board/([A-Z_]{255})/rack/([A-Z]{7})$") $ do
    board <- param "1"
    rack <- param "2"
    text $ mconcat [board, rack]
  notFound $ do
   text "there is no such route."
