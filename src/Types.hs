module Types
    ( Pos
    , Color
    , Shape
    , Piece (..)
    , Size
    , Board (..)
    , pieceColor
    , boardSize, emptyBoard, empty, isEmpty, isFilled, outOfBoard, getColor
    ) where

type Pos = (Int, Int)  -- row, column
type Color = Char  -- Piece color
type Shape = [Pos]  -- Poss of block
data Piece = Piece Color [Shape]  -- Color, Poss of block
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

isEmpty :: Board -> Pos -> Bool
isEmpty board loc = getColor loc board == empty

isFilled :: Board -> Pos -> Bool
isFilled board loc = not $ isEmpty board loc

outOfBoard :: Pos -> Board -> Bool
outOfBoard (r, c) board = r < 0 || c < 0 || r >= boardRow || c >= boardCol
  where (boardRow, boardCol) = boardSize board

getColor :: Pos -> Board -> Color
getColor (r, c) (Board css) = css !! r !! c
