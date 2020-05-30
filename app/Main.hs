{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.HTTP.Types
import Article

main = scotty 3000 $ do
  -- get article (json)
  get "/article" $ do
    json $ Article 13 "caption" "content" -- Call Article constructor and encode the result as JSON

  -- post article (json)
  post "/article" $ do
    article <- jsonData :: ActionM Article -- Decode body of the POST request as an Article object
    json article                           -- Send the encoded object back as JSON
