module Main where

import Parser (parse)
import Solver (solve)

main :: IO ()
main = do
  s <- getContents
  let (rows, cols, pieces, startPoss) = parse s
  print $ solve rows cols pieces startPoss
