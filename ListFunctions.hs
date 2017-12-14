{- |
Module      :  $Header$
Description :  List functions used in Minesweeper game

Maintainer  :  irvin93d@gmail.com, o.henriksson@gmail.com
-}

-- TODO: Export only needed functions


module ListFunctions where

import System.Random

-- | For a given list l, and a given tuple (i, v),
--   replace element i in l with v
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i,x)
    | null l                     = error "(!!=): List is empty."
    | not$inRange 0 i (length l) = error "(!!=): Index out of range."
    | otherwise                  = l1 ++ [x] ++ l2
    where (_, _, l1, l2) = pop i l

(!!!=) :: [[a]] -> (Int,Int,a) -> [[a]]
(!!!=) ll (r,c,x)
    | null ll                    = error "(!!!=): List is empty."
    | not$inRange 0 r (length ll) = error "(!!!=): Index out of range."
    | otherwise = ll !!= (r,l)
    where l = (ll !! r) !!= (c,x)

sumTuple :: (Num a) => (a,a) -> (a,a) -> (a,a)
sumTuple (x1,y1) (x2,y2) = (x1+x2,y1+y2)

-- | For two given lists, return the cartesian product
cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys = [(x,y) | x <- xs, y <- ys]

-- | Count occurances of given a function in given list
count :: (Eq a) => (a -> Bool) -> [a] -> Int
count f = length . filter f

-- | For three given elements, return True if second element
--   is equal to or larger than first, but small than third
inRange :: (Eq a, Ord a) => a -> a -> a -> Bool
inRange a b c = a <= b && b < c

-- | Return input
nop :: a -> a
nop a = a

-- | For a given list l, and a given index i, remove element i from l
--   and return tuple with <element i>, <new list>, <elements before i>,
--   <elements after i>
pop :: Int -> [a] -> (a, [a], [a], [a])
pop i l
    | not$inRange 0 i (length l) = error "pop: Index out of range."
    | otherwise = (head l2, l1 ++ drop 1 l2, l1, drop 1 l2)
    where (l1, l2) = splitAt i l

take3 :: Int -> [a] -> [a]
take3 i = drop (i - 1) . take (i + 2)

-- | Take n random elements from a list.
takeRandom :: StdGen -> Int -> [a] -> [a]
takeRandom _ 0 _  = []
takeRandom _ _ [] = error "takeRandom: Not enough elements."
takeRandom g n l  = x:takeRandom g' (n-1) l'
    where
        (i, g') = randomR (0, length l-1) g
        (x, l', _, _) = pop i l
