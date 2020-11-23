import System.Environment (lookupEnv, getEnv)
import Data.Maybe (fromMaybe)

import App (app)

getPort :: IO Int
getPort = read . fromMaybe "3000" <$> lookupEnv "PORT" -- TODO: make 3000 if PORT is not an integer

main :: IO ()
main = getPort >>= app
