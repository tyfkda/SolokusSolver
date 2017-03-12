module Parser
    ( parse
    -- For unit test
    , parseStartLocation
    ) where

import Data.Text (split, pack, unpack)

type Location = (Int, Int)  -- row, column
type Color = Char  -- Piece color
data Piece = Piece Color Bool Bool [Location]  -- Color, Rotation enable?, Flip enable?, Locations of block
  deriving Show

type Size = (Int, Int)  -- row, column
data Board = Board Size [(Color, Location)]  -- Size, Start positions for each letters.
  deriving Show

parse :: String -> Board
parse contents = Board (rows, cols) startPoints
  where (boardLine : startPointsLine : rs) = lines contents
        [rows, cols] = map read $ words boardLine
        startPoints = map parseStartLocation $ words startPointsLine

splitString sep str = map unpack $ split (== sep) $ pack str

-- ex. "G:1,4" -> ('G', (1, 4))
parseStartLocation :: String -> (Color, Location)
parseStartLocation ss = (col, loc)
  where (col : ':' : rs) = ss
        loc = (\(r : c : _) -> (read r, read c)) $ splitString ',' rs
