module Parser
    ( parse
    -- For unit test
    , parseStartPos
    , parsePiece
    ) where

import Prelude hiding (flip)
import Data.List (nub)
import Data.Text (split, pack, unpack)

import Types (Pos, Color, Shape, Piece (..), Size, Board (..))

parse :: String -> (Int, Int, [Piece], [(Color, Pos)])
parse contents = (rows, cols, pieces, startPoss)
  where (boardLine : startPossLine : rs) = lines contents
        [rows, cols] = map read $ words boardLine
        startPoss = map parseStartPos $ words startPossLine
        pieces = map parsePiece rs

-- ex. "G:1,4" -> ('G', (0, 3))
parseStartPos :: String -> (Color, Pos)
parseStartPos ss = (col, pos)
  where (col : ':' : rs) = ss
        pos = parsePos rs

-- ex. "G . . 1,1, 1,2 1,3 2,2" -> Piece 'G' [[(0,0),(0,1),(0,2),(1,1)]]
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
          | canRotate  = take 4 $ iterate rotate90 shape
          | otherwise  = [shape]
        flipped
          | canFlip    = rotated ++ map flip rotated
          | otherwise  = rotated

rotate90 :: Shape -> Shape
rotate90 shape = normalizeShape $ map rot90 shape
  where rot90 (r, c) = (-c, r)

flip :: Shape -> Shape
flip shape = normalizeShape $ map f shape
  where f (r, c) = (-r, c)

-- Move a shape to fit the origin
normalizeShape :: Shape -> Shape
normalizeShape shape = map (\(r, c) -> (r - r0, c - c0)) shape
  where r0 = minimum $ map fst shape
        c0 = minimum $ map snd shape

parseShape :: [String] -> Shape
parseShape = map parsePos

-- ex. "1,4" -> (0, 3)
parsePos :: String -> Pos
parsePos = (\(r : c : _) -> (read r - 1, read c - 1)) . splitString ','

splitString :: Char -> String -> [String]
splitString sep str = map unpack $ split (== sep) $ pack str
