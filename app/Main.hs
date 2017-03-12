module Main where

import Parser (parse)
import Solver (solve)
import Types (Board (..))

putBoard :: Board -> IO ()
putBoard (Board css) = do
  putStrLn "--------"
  mapM_ putStrLn css

main :: IO ()
main = do
  s <- getContents
  let (rows, cols, pieces, startPoss) = parse s
  let results = solve rows cols pieces startPoss

  putStrLn $ "Result: #" ++ (show $ length results)
  mapM_ putBoard results
