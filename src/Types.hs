module Types
    ( Pos
    , Color
    , Shape
    , Piece (..)
    , Size
    , Board (..)
    , shapeSize
    , pieceColor
    , boardSize, blankBoard, blank, isBlank, isFilled, outOfBoard, getColorAt, canPutShape, putShape
    ) where

type Pos = (Int, Int)  -- row, column

type Color = Char  -- Piece color

blank :: Color
blank = '.'

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

blankBoard :: Int -> Int -> Board
blankBoard rows cols = Board $ replicate rows line
  where line = replicate cols blank

isBlank :: Pos -> Board -> Bool
isBlank loc board = getColorAt loc board == blank

isFilled :: Pos -> Board -> Bool
isFilled loc board = not $ isBlank loc board

outOfBoard :: Pos -> Board -> Bool
outOfBoard (r, c) board = r < 0 || c < 0 || r >= boardRow || c >= boardCol
  where (boardRow, boardCol) = boardSize board

getColorAt :: Pos -> Board -> Color
getColorAt (r, c) (Board css) = css !! r !! c

canPutShape :: Pos -> Shape -> Color -> Board -> Bool
canPutShape (r0, c0) shape col board = not $ any invalid shape
  where invalid (r, c) = outOfBoard pos board ||
                         isFilled pos board ||
                         isShareEdge col pos board
          where pos = (r0 + r, c0 + c)

isShareEdge :: Color -> Pos -> Board -> Bool
isShareEdge col (r, c) board = any isShare adjacents
  where isShare (ar, ac) = not (outOfBoard adj board) && getColorAt adj board == col
          where adj = (r + ar, c + ac)
        adjacents = [(-1, 0), (1, 0), (0, -1), (0, 1)]

putShape :: Color -> Pos -> Shape -> Board -> Board
putShape col (sr, sc) shape board = foldr put board shape
  where put (r, c) (Board colss) = Board $ replace2 colss (sr + r) (sc + c) col

replace :: [a] -> Int -> a -> [a]
replace xs p x = take p xs ++ [x] ++ drop (p + 1) xs

replace2 :: [[a]] -> Int -> Int -> a -> [[a]]
replace2 xss row col x = replace xss row newLine
  where newLine = replace (xss !! row) col x
