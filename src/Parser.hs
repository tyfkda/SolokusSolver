module Parser
    ( parse
    -- For unit test
    , parseStartLocation
    , parsePiece
    ) where

import Prelude hiding (flip)
import Data.List (nub)
import Data.Text (split, pack, unpack)

type Location = (Int, Int)  -- row, column
type Color = Char  -- Piece color
type Shape = [Location]  -- Locations of block
data Piece = Piece Color [Shape]  -- Color, Locations of block
  deriving Show

type Size = (Int, Int)  -- row, column
data Board = Board Size [(Color, Location)]  -- Size, Start positions for each letters.
  deriving Show

parse :: String -> (Board, [Piece])
parse contents = (Board (rows, cols) startPoints, pieces)
  where (boardLine : startPointsLine : rs) = lines contents
        [rows, cols] = map read $ words boardLine
        startPoints = map parseStartLocation $ words startPointsLine
        pieces = map parsePiece rs

-- ex. "G:1,4" -> ('G', (1, 4))
parseStartLocation :: String -> (Color, Location)
parseStartLocation ss = (col, loc)
  where (col : ':' : rs) = ss
        loc = parseLocation rs

-- ex. "G . . 1,1, 1,2 1,3 2,2" -> Piece 'G' [[(1,1),(1,2),(1,3),(2,2)]]
parsePiece :: String -> Piece
parsePiece ss = Piece col (expandShape canRotate canFlip shape)
  where (cs : rs : fs : ls) = words ss
        col = head cs
        canRotate = rs /= "."
        canFlip = fs /= "."
        shape = parseShape ls

expandShape :: Bool -> Bool -> Shape -> [Shape]
expandShape canRotate canFlip shape = nub flipped
  where rotated
          | canRotate  = rotate shape
          | otherwise  = [shape]
        flipped
          | canFlip    = rotated ++ map flip rotated
          | otherwise  = rotated

rotate = undefined
flip = undefined

parseShape :: [String] -> Shape
parseShape = map parseLocation

-- ex. "1,4" -> (1, 4)
parseLocation :: String -> Location
parseLocation = (\(r : c : _) -> (read r, read c)) . splitString ','

splitString :: Char -> String -> [String]
splitString sep str = map unpack $ split (== sep) $ pack str
