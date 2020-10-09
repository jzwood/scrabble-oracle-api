module AesonPractice where

import Data.Aeson.Types    -- that's where Parser comes from
import Data.Aeson
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM
import GHC.Exts


revStrings :: Value -> Value
revStrings (String x) = String (T.reverse x)
revStrings (Array x)  = Array (fmap revStrings x)
revStrings (Object x) = let revPair (k, v) = (T.reverse k, revStrings v)
                        in  Object . fromList . map revPair . HM.toList $ x
revStrings other      = other


main :: IO ()
main = do
  putStr . show $ 3
