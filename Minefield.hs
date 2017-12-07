{- |
Module      :  $Header$
Description :  <optional short text displayed on contents page>
Copyright   :  (c) <Authors or Affiliations>
License     :  <license>

Maintainer  :  irvin93d@gmail.com
Stability   :  unstable | experimental | provisional | stable | frozen
Portability :  portable | non-portable (<reason>)

<module description starting at first column>
-}

module Minefield where

import Data.Maybe
import System.Random

data Content = Numeric Integer | Mine | Empty
               deriving (Eq, Show)

data Status = Open | Closed | Flaged
              deriving (Eq, Show)

data Cell = Cell { content :: Content, status :: Status }
            deriving (Eq, Show)

data Grid = Grid { rows :: [[Cell]], size :: (Int,Int), mines :: Int}
            deriving (Eq, Show)


emptyGrid :: (Int, Int) -> Grid
emptyGrid (w, h) = Grid rows (w,h) 0
   where rows = replicate h $ replicate w $ Cell Empty Closed

makeGrid :: StdGen -> (Int, Int) -> Int -> Grid
makeGrid g (w, h) mines
    | any ((<) 0) [w,h,mines] = error "makeGrid: Negative numbers forbidden."
    | (w*h) <= mines          = error "makeGrid: Too many mines."
    | otherwise = makeGrid' g (emptyGrid (w,h)) mines minePositions
    where minePositions = [(r,c) | r <- [0..(h-1)], c <- [0..(w-1)]]

makeGrid' :: StdGen -> Grid -> Int -> [(Int,Int)] -> Grid
makeGrid' _ grid 0 _  = grid
makeGrid' _ grid _ [] = error "makeGrid: positionList too short."
makeGrid' g grid m mp = update grid' (row, col, Just Mine, Nothing)
    where
        (i, g') = randomR (0, length mp) g
        ((row,col), mp', _, _) = pop i mp
        grid' = makeGrid' g' grid (m-1) mp'

inRange :: (Eq a, Ord a) => a -> a -> a -> Bool
inRange a b c = a <= b && b < c

-- | For a given list l, and a given tuple (i, v),
--   replace element i in l with v
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, r)
    | null l                     = error "(!!): List is empty."
    | not$inRange 0 i (length l) = error "(!!): Index out of range."
    | otherwise                  = l1 ++ [r] ++ l2
    where (_, _, l1, l2) = pop i l

pop :: Int -> [a] -> (a, [a], [a], [a])
pop i l 
    | not$inRange 0 i (length l - 1) = error "pop: Index out of range."
    | otherwise = (head l2, l1 ++ (drop 1 l2), l1, drop 1 l2)
    where (l1, l2) = splitAt i l

-- | For a given Grid grid, and a given tuple (row, col, cont, stat),
--   update cell (row, col) with non-nothing cont and stat
update :: Grid -> (Int, Int, Maybe Content, Maybe Status) -> Grid
update grid (_, _, Nothing, Nothing) = grid
update grid (row, col, cont, stat)
    | not$inRange 0 row $fst(size grid) = error "update: Row out of range."
    | not$inRange 0 col $snd(size grid) = error "update: Column out of range."
    | otherwise = Grid rows' (size grid) (mines grid)
    where
        cell  = rows grid !! row !! col
        cont' = if isJust cont then fromJust cont else content cell
        stat' = if isJust stat then fromJust stat else status cell
        (r, _, rs1, rs2) = pop row $ rows grid
        rows' = rs1 ++ [r !!= (col, Cell cont' stat')] ++ rs2

setCell :: Grid -> (Int, Int) -> Content -> Maybe Grid
setCell grid (row, col) cont
        | status  cell == Open = Nothing
        | content cell == cont = Nothing
        | otherwise = Just $ update grid (row, col, Just cont, Nothing)
    where cell  = rows grid !! row !! col

isLostCell :: Cell -> Bool 
isLostCell cell = status cell == Open && content cell == Mine

isLost :: Grid -> Bool
isLost grid = any isLostCell $ concat $ rows grid

nonMines :: Grid -> Int
nonMines grid = rows * cols - mines grid
    where (rows, cols) = size grid

isOpen :: Cell -> Bool
isOpen cell = status cell == Open

isAllOpen :: Grid -> Bool
isAllOpen grid = length opens == nonMines grid
    where opens = filter isOpen $ concat $ rows grid
