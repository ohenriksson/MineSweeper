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

data Grid = Grid { rows :: [[Cell]], size :: (Int,Int), mines :: Int}
            deriving (Eq, Show)


emptyGrid :: (Int, Int) -> Grid
emptyGrid (w, h) = Grid rows (w,h) 0
   where rows = replicate h $ replicate w $ Cell Empty Closed

-- TODO
-- makeGrid :: (Int, Int) -> Int -> Grid

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
        | otherwise = Grid rows' (size grid) (mines grid)
    where
        cell  = rows grid !! row !! col
        cont' = if isJust cont then fromJust cont else content cell
        stat' = if isJust stat then fromJust stat else status cell
        (rs1, rs2) = splitAt row $ rows grid
        rs' = [head rs2 !!= (col, Cell cont' stat')]
        rows' = rs1 ++ rs' ++ (drop 1 rs2)

setCell :: Grid -> (Int, Int) -> Content -> Maybe Grid
setCell grid (row, col) cont
        | row > fst (size grid) = Nothing
        | col > snd (size grid) = Nothing
        | status cell == Open   = Nothing
        | content cell == cont  = Nothing
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
