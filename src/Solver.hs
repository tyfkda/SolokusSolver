module Solver
    ( solve
    ) where

import Data.List (foldl', inits, nub, tails)
import Data.Maybe (catMaybes)

import Types ( Pos, Color, Shape, Piece (..), Size, Board (..)
             , (.+.), (.-.)
             , boardSize, blankBoard, isBlank, inBoard, getColorAt, pieceColor
             , canPutShape, putShape )
import Util (expandRecur, rotates)

solve :: Size -> [Piece] -> [(Color, Pos)] -> [Board]
solve size pieces startPoss = foldl' f initial startPoss
  where f boards (col, pos) = nub $ concatMap (g col pos) boards
        g col pos board = expandRecur putColoredPieces (board, [pos], coloredPieces col)
        initial = [blankBoard size]
        coloredPieces col = [piece | piece <- pieces, pieceColor piece == col]

putColoredPieces :: (Board, [Pos], [Piece]) -> Either [(Board, [Pos], [Piece])] [Board]
putColoredPieces (board, _, []) = Right [board]
putColoredPieces (_, [], _) = Right []
putColoredPieces (board, (pos: poss), pieces) = Left $ noPut ++ putOnes
  where noPut = [(board, poss, pieces)]
        putOnes = concatMap put1 $ rotates pieces
        put1 (headPiece@(Piece col _) : leftPiece) =
            [(board', enumerateCorners col board', leftPiece) | board' <- put1PieceAt headPiece board pos]

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
