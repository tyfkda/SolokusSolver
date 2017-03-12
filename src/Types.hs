module Types
    ( Location
    , Color
    , Shape
    , Piece (..)
    , Size
    , Board (..)
    , pieceColor
    , boardSize, emptyBoard, empty, isEmpty, isFilled, outOfBoard, getColor
    ) where

type Location = (Int, Int)  -- row, column
type Color = Char  -- Piece color
type Shape = [Location]  -- Locations of block
data Piece = Piece Color [Shape]  -- Color, Locations of block
  deriving Show

pieceColor (Piece col _) = col

type Size = (Int, Int)  -- row, column
data Board = Board [[Color]]  -- Size, Start positions for each letters.
  deriving (Eq, Show)

boardSize :: Board -> Size
boardSize (Board css) = (length css, length $ head css)

emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = Board $ replicate rows line
  where line = replicate cols empty

empty :: Color
empty = '.'

isEmpty :: Board -> Location -> Bool
isEmpty board loc = getColor loc board == empty

isFilled :: Board -> Location -> Bool
isFilled board loc = not $ isEmpty board loc

outOfBoard :: Location -> Board -> Bool
outOfBoard (r, c) board = r < 0 || c < 0 || r >= boardRow || c >= boardCol
  where (boardRow, boardCol) = boardSize board

getColor :: Location -> Board -> Color
getColor (r, c) (Board css) = css !! r !! c
