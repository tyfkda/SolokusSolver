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
  where f (col, pos) boards = putPermColored col [pos] boards $ coloredPieces col
        initial = [blankBoard size]
        coloredPieces col = [piece | piece <- pieces, pieceColor piece == col]

putPermColored :: Color -> [Pos] -> [Board] -> [Piece] -> [Board]
putPermColored col poss boards pieces = concatMap (putColored col bss') allOrder
  where allOrder = permutations pieces
        bss' = zip boards $ repeat poss

putColored :: Color -> [(Board, [Pos])] -> [Piece] -> [Board]
putColored col bss pieces = nub $ map fst $ foldr f bss pieces
  where f piece bss' = [(board, enumerateCorners col board) | board <- nextBoards]
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
                         any (\offset -> isCornerFor col pos offset board) diagonals
  where diagonals = [(-1, -1), (1, -1), (-1, 1), (1, 1)]

isCornerFor :: Color -> Pos -> Pos -> Board -> Bool
isCornerFor col pos offset@(or, oc) board =
    inBoard (pos .+. offset) board &&
    getColorAt (pos .+. offset) board == col &&
    getColorAt (pos .+. (0, oc)) board /= col &&
    getColorAt (pos .+. (or, 0)) board /= col
