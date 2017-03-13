module Parser
    ( parse
    -- For unit test
    , parseStartPos
    , parsePiece
    ) where

import Data.List (nub, sort)
import Data.Text (split, pack, unpack)

import Types ( Pos, Color, Shape, Piece (..), Size
             , (.-.)
             )

parse :: String -> (Size, [Piece], [(Color, Pos)])
parse contents = ((rows, cols), pieces, startPoss)
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
parsePiece ss = Piece col $ expandShape canRotate canFlip shape
  where (cs : rs : fs : ls) = words ss
        col = head cs
        canRotate = rs /= "."
        canFlip = fs /= "."
        shape = parseShape ls

expandShape :: Bool -> Bool -> Shape -> [Shape]
expandShape canRotate canFlip shape = nub $ [normalizeShape (f shape) | f <- modifiers]
  where modifiers = [ff . fr | ff <- flipFuncs, fr <- rotFuncs]
        rotFuncs
          | canRotate  = take 4 $ iterate (rotate90 .) id
          | otherwise  = [id]
        flipFuncs
          | canFlip    = [id, flipHorz]
          | otherwise  = [id]

rotate90 :: Shape -> Shape
rotate90 shape = map rot90 shape
  where rot90 (r, c) = (-c, r)

flipHorz :: Shape -> Shape
flipHorz shape = map f shape
  where f (r, c) = (-r, c)

-- Move a shape to fit the origin
normalizeShape :: Shape -> Shape
normalizeShape shape = map (.-. basePos) shape
  where basePos = (minimum $ map fst shape, minimum $ map snd shape)

parseShape :: [String] -> Shape
parseShape = map parsePos

-- ex. "1,4" -> (0, 3)
parsePos :: String -> Pos
parsePos = (\(r : c : _) -> (read r - 1, read c - 1)) . splitString ','

splitString :: Char -> String -> [String]
splitString sep str = map unpack $ split (== sep) $ pack str
