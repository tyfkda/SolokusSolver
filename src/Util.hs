module Util
    ( expandRecur
    , minimumOf, maximumOf
    , replace, replace2
    , rotates
    , splitString
    ) where

import Data.Text (split, pack, unpack)

expandRecur :: Foldable t => (a -> Either (t a) [b]) -> a -> [b]
expandRecur f = g . f
  where g (Left as) = concatMap (expandRecur f) as
        g (Right b) = b

minimumOf :: (Foldable t, Functor t, Ord b) => (a -> b) -> t a -> b
minimumOf = xxxOf minimum

maximumOf :: (Foldable t, Functor t, Ord b) => (a -> b) -> t a -> b
maximumOf = xxxOf maximum

replace :: [a] -> Int -> a -> [a]
replace xs p x = take p xs ++ [x] ++ drop (p + 1) xs

replace2 :: [[a]] -> Int -> Int -> a -> [[a]]
replace2 xss row col x = replace xss row newLine
  where newLine = replace (xss !! row) col x

rotates xs = take (length xs) $ iterate rotate xs
rotate (x:xs) = xs ++ [x]

splitString :: Char -> String -> [String]
splitString sep str = map unpack $ split (== sep) $ pack str

xxxOf :: (Foldable t, Functor t, Ord b) => (t b -> b) -> (a -> b) -> t a -> b
xxxOf agg f = agg . fmap f
