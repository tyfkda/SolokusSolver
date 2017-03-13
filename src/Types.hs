module Types
    ( Pos
    , Color
    , Shape
    , Piece (..)
    , Size
    , Board (..)
    , (.+.), (.-.)
    , shapeSize
    , pieceColor
    , boardSize, blankBoard, blank, isBlank, isFilled, outOfBoard, getColorAt, canPutShape, putShape
    ) where

type Pos = (Int, Int)  -- row, column

(.+.) (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)
(.-.) (r1, c1) (r2, c2) = (r1 - r2, c1 - c2)

type Size = (Int, Int)  -- row, column

outOfSize :: Pos -> Size -> Bool
outOfSize (r, c) (rSize, cSize) = r < 0 || c < 0 || r >= rSize || c >= cSize

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
outOfBoard pos = outOfSize pos . boardSize

getColorAt :: Pos -> Board -> Color
getColorAt (r, c) (Board css) = css !! r !! c

canPutShape :: Pos -> Shape -> Color -> Board -> Bool
canPutShape basePos shape col board = fitInBoard && (not $ any invalid shape)
  where fitInBoard = not $ any (flip outOfBoard board) [basePos,
                                                        basePos .+. shapeSize shape .-. (1, 1)]
        invalid offset = isFilled pos board ||
                         isShareEdge col pos board
          where pos = basePos .+. offset

isShareEdge :: Color -> Pos -> Board -> Bool
isShareEdge col basePos board = any isShare adjacents
  where isShare offset = not (outOfBoard adj board) && getColorAt adj board == col
          where adj = basePos .+. offset
        adjacents = [(-1, 0), (1, 0), (0, -1), (0, 1)]

putShape :: Color -> Pos -> Shape -> Board -> Board
putShape col basePos shape board = foldr put board shape
  where put offset (Board colss) = Board $ replace2 colss r c col
          where (r, c) = basePos .+. offset

replace :: [a] -> Int -> a -> [a]
replace xs p x = take p xs ++ [x] ++ drop (p + 1) xs

replace2 :: [[a]] -> Int -> Int -> a -> [[a]]
replace2 xss row col x = replace xss row newLine
  where newLine = replace (xss !! row) col x
