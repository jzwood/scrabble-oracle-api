module EmailTemplate where

import Game.SingleBestPlay
import Game.ScrabbleBoard
import qualified Data.Matrix as Mat

boardToMarkup :: Board -> String
boardToMarkup board =
  "<table style='border: 2px solid black; padding: 4px; margin: 12px auto; text-align: center; line-height: 1;'><tbody>" ++
  (unlines . map row $ Mat.toLists $ easyReadBoard board) ++
  "</tbody></table>"
    where
      row cs = "<tr>" ++ concatMap (\c -> "<td>" ++ if c == ' ' then "&nbsp;" else c : "</td>") cs ++ "</tr>"

wordToMarkup :: String -> String
wordToMarkup word = "<div><strong>WORD:</strong> " ++ word ++ "</div>"

scoreToMarkup :: Score -> String
scoreToMarkup score = "<div><strong>SCORE:</strong> " ++ show score ++ "</div>"

emailTemplate :: Board -> String -> Score -> String
emailTemplate board word score = unlines
  [ "<!DOCTYPE HTML PUBLIC '-//W3C//DTD HTML 4.01 Transitional//EN'>"
  , "<html>"
  ,   "<head>"
  ,     "<title>Scrabble Oracle</title>"
  ,     "<meta name='viewport' content='width=device-width'>"
  ,     "<meta http-equiv='content-type' content='text/html;charset=utf-8'>"
  ,     "<style>"
  ,       "td { padding: 0 3px; }"
  ,     "</style>"
  ,   "</head>"
  ,   "<body style='color: #333;'>"
  ,     "<section style='font-family: Arial, sans-serif; text-align: center;'>"
  ,       "<h3>THE SCRABBLE ORACLE</h3>"
  ,       boardToMarkup board
  ,       wordToMarkup word
  ,       scoreToMarkup score
  ,     "</section>"
  ,   "</body>"
  , "</html>"
  ]
