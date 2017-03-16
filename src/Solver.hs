module Solver
    ( solve
    ) where

import Data.List (inits, nub, permutations, tails)
import Data.Maybe (catMaybes)

import Types ( Pos, Color, Shape, Piece (..), Size, Board (..)
             , (.+.), (.-.)
             , boardSize, blankBoard, isBlank, inBoard, getColorAt, pieceColor
             , canPutShape, putShape )

solve :: Size -> [Piece] -> [(Color, Pos)] -> [Board]
solve size pieces startPoss = foldr f initial startPoss
  where f (col, pos) boards = putPermColored col pos boards $ coloredPieces col
        initial = [blankBoard size]
        coloredPieces col = [piece | piece <- pieces, pieceColor piece == col]

putPermColored :: Color -> Pos -> [Board] -> [Piece] -> [Board]
putPermColored col pos boards pieces = concatMap (putColored col pos boards) allOrder
  where allOrder = permutations pieces

putColored :: Color -> Pos -> [Board] -> [Piece] -> [Board]
putColored col pos boards pieces = nub $ map fst $ foldr f initial pieces
  where initial = [(b, [pos]) | b <- boards]
        f piece bss' = [(board, enumerateCorners col board) | board <- nextBoards]
          where nextBoards = putColored1 col bss' piece

putColored1 :: Color -> [(Board, [Pos])] -> Piece -> [Board]
putColored1 col bss piece = concatMap (uncurry $ putPoss piece) bss
  where putPoss piece board poss = concatMap (put1PieceAt piece board) poss

put1PieceAt :: Piece -> Board -> Pos -> [Board]
put1PieceAt (Piece col shapes) board basePos = concatMap (putShapeAt col basePos board) shapes

putShapeAt :: Color -> Pos -> Board -> Shape -> [Board]
putShapeAt col basePos board shape =
    [putShape col (basePos .-. offset) shape board | offset <- shape,
                                                     canPut offset]
  where canPut offset = canPutShape (basePos .-. offset) shape col board

enumerateCorners :: Color -> Board -> [Pos]
enumerateCorners col board = [pos | pos <- allPoss, isCorner col pos board]
  where allPoss = [(r, c) | r <- [0..boardRows - 1], c <- [0..boardCols - 1]]
        (boardRows, boardCols) = boardSize board

isCorner :: Color -> Pos -> Board -> Bool
isCorner col pos board = isBlank pos board &&
                         any (isCornerFor col pos board) diagonals
  where diagonals = [(-1, -1), (1, -1), (-1, 1), (1, 1)]

isCornerFor :: Color -> Pos -> Board -> Pos -> Bool
isCornerFor col pos board offset@(or, oc) =
    inBoard (pos .+. offset) board &&
    getColorAt (pos .+. offset) board == col &&
    getColorAt (pos .+. (0, oc)) board /= col &&
    getColorAt (pos .+. (or, 0)) board /= col
