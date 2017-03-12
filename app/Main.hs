module Main where

import Parser (parse)
import Solver (solve)

main :: IO ()
main = do
  s <- getContents
  let (rows, cols, pieces, startLocations) = parse s
  print $ solve rows cols pieces startLocations
