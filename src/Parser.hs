module Parser
    ( parse
    -- For unit test
    , parseStartLocation
    , parsePiece
    ) where

import Prelude hiding (flip)
import Data.List (nub)
import Data.Text (split, pack, unpack)

import Types (Location, Color, Shape, Piece (..), Size, Board (..))

parse :: String -> (Int, Int, [Piece], [(Color, Location)])
parse contents = (rows, cols, pieces, startPoints)
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

-- ex. "1,4" -> (0, 3)
parseLocation :: String -> Location
parseLocation = (\(r : c : _) -> (read r - 1, read c - 1)) . splitString ','

splitString :: Char -> String -> [String]
splitString sep str = map unpack $ split (== sep) $ pack str
