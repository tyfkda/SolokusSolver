module Types
    ( Location
    , Color
    , Shape
    , Piece (..)
    , Size
    , Board (..)
    ) where

type Location = (Int, Int)  -- row, column
type Color = Char  -- Piece color
type Shape = [Location]  -- Locations of block
data Piece = Piece Color [Shape]  -- Color, Locations of block
  deriving Show

type Size = (Int, Int)  -- row, column
data Board = Board [[Color]]  -- Size, Start positions for each letters.
  deriving Show
