module Solver
    ( solve
    ) where

import Data.List (inits, nub, permutations, tails)
import Data.Maybe (catMaybes)

import Types ( Pos, Color, Shape, Piece (..), Size, Board (..)
             , boardSize, emptyBoard, isEmpty, isFilled, outOfBoard, getColor, pieceColor
             , canPutShape, putShape )

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
findCorners col board@(Board css) = filter meetCorner allPoss
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
