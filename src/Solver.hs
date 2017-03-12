module Solver
    ( solve
    -- For unit test
    ) where

import Data.List (inits, nub, permutations, tails)
import Data.Maybe (catMaybes)

import Types (Pos, Color, Shape, Piece (..), Size, Board (..),
              boardSize, emptyBoard, isEmpty, isFilled, outOfBoard, getColor, pieceColor)

solve :: Int -> Int ->  [Piece] ->  [(Color, Pos)] -> [Board]
solve rows cols pieces startPoss = nub $ foldr f initial startPoss
  where coloredPieces col = filter ((== col) . pieceColor) pieces
        initial = [(emptyBoard rows cols)]
        f (col, pos)  boards = map fst $ putPermColored col (map (\board -> (board, [pos])) boards) $ coloredPieces col

putPermColored :: Color -> [(Board, [Pos])] -> [Piece] -> [(Board, [Pos])]
putPermColored col bss pieces = concatMap (putColored col bss) allOrder
  where allOrder = permutations pieces

putColored :: Color -> [(Board, [Pos])] -> [Piece] -> [(Board, [Pos])]
putColored col bss pieces = foldr (\piece bss' -> putColored1 col bss' piece) bss pieces

putColored1 :: Color -> [(Board, [Pos])] -> Piece -> [(Board, [Pos])]
putColored1 col bss piece = map f $ concatMap (\(board, poss) -> putted board poss) bss
  where putted board poss = putPoss piece poss board
        putPoss piece poss board = concatMap (\pos -> put1PieceAt pos piece board) poss
        f board = (board, findCorners col board)

findCorners :: Color -> Board -> [Pos]
findCorners col board@(Board css) = nub $ filter meetCorner allPoss
  where allPoss = [(r, c) | r <- [0..boardRows - 1], c <- [0..boardCols - 1]]
        (boardRows, boardCols) = boardSize board
        meetCorner pos@(r, c) = isEmpty board pos && any (meetCornerFor pos) fourCorners
        meetCornerFor pos@(r, c) offset@(or, oc) = not (outOfBoard (r + or, c + oc) board) && isCorner pos offset
        isCorner pos@(r, c) offset@(or, oc) = getColor (r + or, c + oc) board == col &&
                                              getColor (r, c + oc) board /= col &&
                                              getColor (r + or, c) board /= col
        fourCorners = [(-1, -1), (1, -1), (-1, 1), (1, 1)]

put1PieceAt ::Pos -> Piece ->  Board -> [Board]
put1PieceAt (sr, sc) (Piece col shapes) board = oks
  where oks = catMaybes $ concatMap (\shape -> map (put shape) shape) shapes
        put shape (r, c) | canPutShape (sr - r, sc - c) shape col board  = Just (putShape col (sr - r, sc -c) shape board)
                         | otherwise                                     = Nothing

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

shapeSize :: Shape -> Size
shapeSize ls = (maxRow + 1, maxCol + 1)
  where maxRow = maximum $ map fst ls
        maxCol = maximum $ map snd ls
