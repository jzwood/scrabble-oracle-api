module EmailTemplate where

import Game.SingleBestPlay
import Game.ScrabbleBoard
import qualified Data.Matrix as Mat

boardToMarkup :: Board -> String
boardToMarkup board =
  "<table style='border: 1px solid black; padding: 2px; margin: 12px auto; text-align: center; line-height: 1;'><tbody>" ++
  (unlines . map row $ Mat.toLists $ easyReadBoard board) ++
  "</tbody></table>"
    where
      row cs = "<tr>" ++ concatMap (\c -> "<td style='padding: 0 3px;'>" ++ if c == ' ' then "&nbsp;" else c : "</td>") cs ++ "</tr>"

wordToMarkup :: String -> String
wordToMarkup word = "<div><strong>WORD:</strong> " ++ word ++ "</div>"

scoreToMarkup :: Score -> String
scoreToMarkup score = "<div><strong>SCORE:</strong> " ++ show score ++ "</div>"

emailTemplate :: Board -> String -> Score -> String
emailTemplate board word score = unlines
  [ "<div style='font-family: Arial, sans-serif; text-align: center;'>"
  ,   "<div><strong>THE SCRABBLE ORACLE</strong></div>"
  ,     boardToMarkup board
  ,     wordToMarkup word
  ,     scoreToMarkup score
  ,   "</div>"
  , "</div>"
  ]
