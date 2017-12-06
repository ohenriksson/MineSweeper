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

data Content = Numeric Integer | Mine | Empty
               deriving (Eq, Show)

data Status = Open | Closed | Flaged
              deriving (Eq, Show)

data Cell = Cell { content :: Content, status :: Status }
            deriving (Eq, Show)

data Grid = Grid { rows :: [[Cell]], size :: (Int,Int)}
            deriving (Eq, Show)


emptyGrid :: (Int, Int) -> Grid
emptyGrid (w, h) = Grid rows (w,h)
   where rows = replicate h $ replicate w $ Cell Empty Closed

-- | For a given list l, and a given tuple (i, v),
--   replace element i in l with v
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, r)
        | null l        = error "(!!): list is empty"
        | i < 0         = error "(!!): index is negative"
        | i >= length l = error "(!!): index out of range"
        | otherwise     = l1 ++ [r] ++ drop 1 l2
    where (l1, l2) = splitAt i l

-- | For a given Grid grid, and a given tuple (row, col, cont, stat),
--   update cell (row, col) with non-nothing cont and stat
update :: Grid -> (Int, Int, Maybe Content, Maybe Status) -> Grid
update grid (_, _, Nothing, Nothing) = grid
update grid (row, col, cont, stat)
        | row > fst (size grid) = error "update: Row out of range."
        | col > snd (size grid) = error "update: Column out of range."
        | otherwise = Grid (rs1 ++ rs' ++ (drop 1 rs2)) (size grid)
    where
        cell  = rows grid !! row !! col
        cont' = if isJust cont then fromJust cont else content cell
        stat' = if isJust stat then fromJust stat else status cell
        (rs1, rs2) = splitAt row $ rows grid
        rs' = [head rs2 !!= (col, Cell cont' stat')]
