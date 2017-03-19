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
    , boardSize, blankBoard, blank, isBlank, isFilled, outOfBoard, inBoard, getColorAt, canPutShape, putShape
    ) where

import Util (maximumOf, replace2)

type Pos = (Int, Int)  -- row, column

(.+.) (r1, c1) (r2, c2) = (r1 + r2, c1 + c2)
(.-.) (r1, c1) (r2, c2) = (r1 - r2, c1 - c2)

type Size = Pos

inSize :: Pos -> Size -> Bool
inSize (r, c) (rSize, cSize) = r >= 0 && c >= 0 && r < rSize && c < cSize

type Color = Char  -- Piece color

blank :: Color
blank = '.'

type Shape = [Pos]  -- Poss of block

shapeSize :: Shape -> Size
shapeSize size = (maximumOf fst size + 1, maximumOf snd size + 1)

data Piece = Piece Color [Shape]  -- Color, Poss of block
  deriving Show

pieceColor :: Piece -> Color
pieceColor (Piece col _) = col

data Board = Board [[Color]]  -- Size, Start positions for each letters.
  deriving (Eq, Show)

boardSize :: Board -> Size
boardSize (Board css) = (length css, length $ head css)

blankBoard :: Size -> Board
blankBoard (rows, cols) = Board $ replicate rows line
  where line = replicate cols blank

isBlank :: Pos -> Board -> Bool
isBlank loc board = getColorAt loc board == blank

isFilled :: Pos -> Board -> Bool
isFilled loc board = not $ isBlank loc board

inBoard :: Pos -> Board -> Bool
inBoard pos = inSize pos . boardSize

outOfBoard :: Pos -> Board -> Bool
outOfBoard = (not .) . inBoard

getColorAt :: Pos -> Board -> Color
getColorAt (r, c) (Board css) = css !! r !! c

canPutShape :: Pos -> Shape -> Color -> Board -> Bool
canPutShape basePos shape col board = fitRectInBoard && all canPut shape
  where fitRectInBoard = all (flip inBoard board) [basePos,
                                                   basePos .+. shapeSize shape .-. (1, 1)]
        canPut offset = isBlank pos board &&
                        not (isShareEdge col pos board)
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
