module Types
    ( Pos
    , Color
    , Shape
    , Piece (..)
    , Size
    , Board (..)
    , shapeSize
    , pieceColor
    , boardSize, emptyBoard, empty, isEmpty, isFilled, outOfBoard, getColor, canPutShape, putShape
    ) where

type Pos = (Int, Int)  -- row, column
type Color = Char  -- Piece color
type Shape = [Pos]  -- Poss of block

shapeSize :: Shape -> Size
shapeSize ls = (maxRow + 1, maxCol + 1)
  where maxRow = maximum $ map fst ls
        maxCol = maximum $ map snd ls

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

canPutShape :: Pos -> Shape -> Color -> Board -> Bool
canPutShape (sr, sc) shape col board = not $ any ng shape
  where ng (r, c) = out || filled || shareEdge
          where pos = (sr + r, sc + c)
                out = outOfBoard pos board
                filled = isFilled board pos
                shareEdge = any isShare [(-1, 0), (1, 0), (0, -1), (0, 1)]
                  where isShare (ar, ac) = not (outOfBoard adj board) && getColor adj board == col
                          where adj = (sr + r + ar, sc + c + ac)

putShape :: Color -> Pos -> Shape -> Board -> Board
putShape col (sr, sc) shape board = foldr put board shape
  where put (r, c) (Board colss) = Board $ replace2 colss (sr + r) (sc + c) col

replace :: [a] -> Int -> a -> [a]
replace xs p x = take p xs ++ [x] ++ drop (p + 1) xs

replace2 :: [[a]] -> Int -> Int -> a -> [[a]]
replace2 xss row col x = replace xss row newLine
  where newLine = replace (xss !! row) col x
