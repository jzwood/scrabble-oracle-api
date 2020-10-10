{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.HTTP.Types

main = scotty 3000 $ do
  get "/" $ do                         -- handle GET request on "/" URL
    text "This was a GET request!"     -- send 'text/plain' response
  delete "/" $ do
    html "This was a DELETE request!"  -- send 'text/html' response
  post "/" $ do
    text "This was a POST request!"
  put "/" $ do
    text "This was a PUT request!"
  -- set a header:
  post "/set-headers" $ do
   status status302  -- Respond with HTTP 302 status code
   setHeader "Location" "http://www.google.com.au"
  -- named parameters:
  get "/askfor/:word" $ do
    w <- param "word"
    html $ mconcat ["<h1>You asked for ", w, ", you got it!</h1>" ]
  -- unnamed parameters from a query string or a form:
  post "/submit" $ do  -- e.g. http://server.com/submit?name=somename
   name <- param "name"
   text name
  -- match a route regardless of the method
  matchAny "/all" $ do
   text "matches all methods"
  -- handler for when there is no matched route
  -- (this should be the last handler because it matches all routes)
  notFound $ do
   text "there is no such route."
