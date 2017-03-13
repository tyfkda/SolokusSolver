module Main where

import System.Console.ANSI

import Parser (parse)
import Solver (solve)
import Types (Board (..))

type Color2 = (ColorIntensity, Color)

resetColor :: IO ()
resetColor = do
  setSGR[]

setColor :: Color2 -> Color2 -> IO ()
setColor (fgi, fg) (bgi, bg) = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]

colorStr :: Color2 -> Color2 -> String -> IO ()
colorStr fc bc str = do
  setColor fc bc
  putStr str

vividWhite = (Vivid, White)
dullWhite = (Dull, White)
dullRed = (Dull, Red)
dullGreen = (Dull, Green)
dullYellow = (Dull, Yellow)
dullBlue = (Dull, Blue)
dullBlack = (Dull, Black)

putCell c = do
  let col = case c of
              'R'  -> dullRed
              'G'  -> dullGreen
              'B'  -> dullBlue
              'Y'  -> dullYellow
              _    -> dullBlack
  colorStr dullWhite col [c]

putBoard :: Board -> IO ()
putBoard (Board css) = do
  resetColor
  putStrLn "--------"
  mapM_ (\row -> mapM_ putCell row >> putStr "\n") css

main :: IO ()
main = do
  s <- getContents
  let (rows, cols, pieces, startPoss) = parse s
  let results = solve rows cols pieces startPoss

  putStrLn $ "Result: #" ++ (show $ length results)
  mapM_ putBoard results
