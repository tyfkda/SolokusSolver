module Solver
    ( solve
    -- For unit test
    ) where

import Data.Maybe (catMaybes)

import Types (Location, Color, Shape, Piece (..), Size, Board (..))

--solve :: Int -> Int ->  [Piece] ->  [(Color, Location)] -> [Board]
solve rows cols pieces startLocations = initialBoards [emptyBoard rows cols] pieces startLocations

-- Just fill start locations.
--initialBoards :: [Board] ->  [Piece] ->  [(Color, Location)] -> [Board]  -- [(Board, [Piece])]
initialBoards boards pieces ss = foldr put boards ss
  where put (col, loc) boards' = concatMap (\piece -> concatMap (enumerateFitBoards loc piece) boards') targetPieces
          where targetPieces = filter matchColor pieces
                matchColor (Piece c _) = c == col

enumerateFitBoards ::Location -> Piece ->  Board -> [Board]
enumerateFitBoards (sr, sc) (Piece col shapes) board = oks
  where oks = catMaybes $ concatMap (\shape -> map (put shape) shape) shapes
        put shape (r, c) | canPutShape board (sr - r, sc - c) shape  = Just (putShape col (sr - r, sc -c) shape board)
                         | otherwise                                 = Nothing

canPutShape :: Board -> Location -> Shape -> Bool
canPutShape board (sr, sc) shape = not $ any (\(r, c) -> outOfRange (sr + r, sc + c) || filled (r, c)) shape
  where outOfRange (rr, cc) = rr < 0 || cc < 0 || rr >= boardRow || cc >= boardCol
        filled loc = isFilled board loc
        (boardRow, boardCol) = boardSize board

putShape :: Color -> Location -> Shape -> Board -> Board
putShape col (sr, sc) shape board = foldr put board shape
  where put (r, c) (Board colss) = Board $ replace2 colss (sr + r) (sc + c) col

replace :: [a] -> Int -> a -> [a]
replace xs p x = take p xs ++ [x] ++ drop (p + 1) xs

replace2 :: [[a]] -> Int -> Int -> a -> [[a]]
replace2 xss row col x = replace xss row newLine
  where newLine = replace (xss !! row) col x

shapeSize :: Shape -> Size
shapeSize ls = (maxRow + 1, maxCol + 1)
  where maxRow = maximum $ map fst ls
        maxCol = maximum $ map snd ls

emptyBoard :: Int -> Int -> Board
emptyBoard rows cols = Board $ replicate rows line
  where line = replicate cols empty

empty :: Color
empty = '.'

isEmpty :: Board -> Location -> Bool
isEmpty (Board colss) (r, c) = colss !! r !! c == empty

isFilled :: Board -> Location -> Bool
isFilled board loc = not $ isEmpty board loc

boardSize :: Board -> Size
boardSize (Board css) = (length css, length $ head css)
