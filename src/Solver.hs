module Solver
    ( solve
    -- For unit test
    ) where

import Data.List (inits, nub, tails)
import Data.Maybe (catMaybes)

import Types (Location, Color, Shape, Piece (..), Size, Board (..),
              boardSize, emptyBoard, isEmpty, isFilled, outOfBoard, getColor)

solve :: Int -> Int ->  [Piece] ->  [(Color, Location)] -> [Board]
solve rows cols pieces startLocations = concatMap putRemainingPieces initialBoards
  where initialBoards = foldr put [(emptyBoard rows cols, pieces)] startLocations
        put (col, loc) =  concatMap (putLocation col loc)
        putRemainingPieces (board, pieces) = foldr f [board] pieces
        f piece@(Piece col shape) boards = concatMap (\board -> concatMap (\loc -> enumerateFitBoards loc piece board) (findCorners col board)) boards

findCorners :: Color -> Board -> [Location]
findCorners col board@(Board css) = nub $ filter meetCorner allLocations
  where allLocations = [(r, c) | r <- [0..boardRows - 1], c <- [0..boardCols - 1]]
        (boardRows, boardCols) = boardSize board
        meetCorner loc@(r, c) = isEmpty board loc && any (meetCornerFor loc) fourCorners
        meetCornerFor loc@(r, c) offset@(or, oc) = not (outOfBoard (r + or, c + oc) board) && isCorner loc offset
        isCorner loc@(r, c) offset@(or, oc) = getColor (r + or, c + oc) board == col &&
                                              getColor (r, c + oc) board /= col &&
                                              getColor (r + or, c) board /= col
        fourCorners = [(-1, -1), (1, -1), (-1, 1), (1, 1)]


putLocation :: Color -> Location -> (Board, [Piece]) -> [(Board, [Piece])]
putLocation col loc (board, pieces) = concatMap put targetPieces
  where targetPieces = filter ((\(Piece c _) -> c == col) . fst) pss
        pss = zipWith (\is ts -> (last is, init is ++ ts)) iss tss
        iss = tail $ inits pieces
        tss = tail $ tails pieces
        put (piece, rest) = zip (enumerateFitBoards loc piece board) $ repeat rest

enumerateFitBoards ::Location -> Piece ->  Board -> [Board]
enumerateFitBoards (sr, sc) (Piece col shapes) board = oks
  where oks = catMaybes $ concatMap (\shape -> map (put shape) shape) shapes
        put shape (r, c) | canPutShape board (sr - r, sc - c) shape  = Just (putShape col (sr - r, sc -c) shape board)
                         | otherwise                                 = Nothing

canPutShape :: Board -> Location -> Shape -> Bool
canPutShape board (sr, sc) shape = not $ any (\(r, c) -> outOfBoard (sr + r, sc + c) board || filled (sr + r, sc + c)) shape
  where filled loc = isFilled board loc

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
