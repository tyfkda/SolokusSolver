module Main where

import System.Console.ANSI

import Parser (parse)
import Solver (solve)
import Types (Board (..))

type ColorPair = (ColorIntensity, Color)

resetColor :: IO ()
resetColor = do
  setSGR []

setColor :: ColorPair -> ColorPair -> IO ()
setColor (fgi, fg) (bgi, bg) = do
  setSGR [SetColor Foreground fgi fg, SetColor Background bgi bg]

colorStr :: ColorPair -> ColorPair -> String -> IO ()
colorStr fc bc str = do
  setColor fc bc
  putStr str

vividWhite = (Vivid, White)
vividRed = (Vivid, Red)
vividGreen = (Vivid, Green)
vividYellow = (Vivid, Yellow)
vividBlue = (Vivid, Blue)
dullBlack = (Dull, Black)

chooseColor 'R' = vividRed
chooseColor 'G' = vividGreen
chooseColor 'B' = vividBlue
chooseColor 'Y' = vividYellow
chooseColor _   = vividWhite

putBoard :: Board -> IO ()
putBoard (Board css) = do
  resetColor
  putStrLn "--------"
  mapM_ putRow css

  where putRow row = do
          mapM_ putCell row
          resetColor
          putStr "\n"

        putCell c = do
          colorStr dullBlack (chooseColor c) [c]

main :: IO ()
main = do
  s <- getContents
  let (size, pieces, startPoss) = parse s
  let results = solve size pieces startPoss

  putStrLn $ "Result: #" ++ (show $ length results)
  mapM_ putBoard results
