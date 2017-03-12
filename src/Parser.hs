module Parser
    ( parse
    -- For unit test
    , parseStartLocation
    , parsePiece
    ) where

import Data.Text (split, pack, unpack)

type Location = (Int, Int)  -- row, column
type Color = Char  -- Piece color
data Piece = Piece Color Bool Bool [Location]  -- Color, Rotation enable?, Flip enable?, Locations of block
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

splitString sep str = map unpack $ split (== sep) $ pack str

-- ex. "G:1,4" -> ('G', (1, 4))
parseStartLocation :: String -> (Color, Location)
parseStartLocation ss = (col, loc)
  where (col : ':' : rs) = ss
        loc = parseLocation rs

-- ex. "G . . 1,1, 1,2 1,3 2,2" -> Piece 'G' False False [(1,1),(1,2),(1,3),(2,2)]
parsePiece :: String -> Piece
parsePiece ss = Piece col rotate flip locs
  where (cs : rs : fs : ls) = words ss
        col = head cs
        rotate = rs /= "."
        flip = fs /= "."
        locs = map parseLocation ls

-- ex. "1,4" -> (1, 4)
parseLocation :: String -> Location
parseLocation = (\(r : c : _) -> (read r, read c)) . splitString ','
