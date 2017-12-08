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
import ListFunctions

-- | Representation of a coordinate in Minefield
data Cell = Cell { content :: Content, status :: Status }
            deriving (Eq)

-- | Ascii representation of cell, always 3 characters
instance Show Cell where
    show (Cell  _          Closed) = "[ ]"
    show (Cell  _          Flagged) = "[f]"
    show (Cell  Empty      Open)   = "   "
    show (Cell  Mine       Open)   = "}#{"
    show (Cell (Numeric n) Open)   = " " ++ show n ++ " "

-- | Representation of whether a Mine is in cell or close
data Content = Numeric Integer | Mine | Empty
               deriving (Eq)

-- | Representation of whether cell is open, closed or flaged by player
data Status = Open | Closed | Flagged
              deriving (Eq)

-- | Representation of a Minefield
data Grid = Grid { rows :: [[Cell]], size :: (Int,Int), mines :: Int}
            deriving (Eq)

-- | Ascii representation of minefield
instance Show Grid where
    show grid = 
        "   " ++ concat (bar $snd $size grid)
        ++ "\n"
        ++ concat[fst r 
        ++ snd r ++ "\n"
        | r <- bar (fst $size grid) `zip`
        [concatMap show r | r <- rows grid] ]

-- | Make size strings of increamenting numbers, each string of 3 characters 
bar :: Int -> [String]
bar size = 
    [if n >= 100 then show n
            else (
                if n >= 10 then show n
                else " " ++ show n) ++ " "
            | n <- [1..size]]


-- | Given a size (width, height), create a grid with no mines
emptyGrid :: (Int, Int) -> Grid
emptyGrid (h, w) = Grid rows (h, w) 0
   where rows = replicate h $ replicate w $ Cell Empty Closed

-- | Test if all non-mines has been open
isAllOpen :: Grid -> Bool
isAllOpen grid = all ((==) Open . status) nonMines
    where nonMines = filter ((/=) Mine . content) $ concat $ rows grid

-- | Test if any mine has been open
isLost :: Grid -> Bool
isLost grid = any ((==) Open . status) mines
    where mines = filter ((==) Mine . content) $ concat $ rows grid

-- | Given an StdGen, a size, and a number of mines, make a random Minefield
makeGrid :: StdGen -> (Int, Int) -> Int -> Grid
makeGrid g (h, w) mines
    | any (0 >) [w,h,mines] = error "makeGrid: Negative numbers forbidden."
    | (w*h) <= mines          = error "makeGrid: Too many mines."
    | otherwise = makeGridMines g (emptyGrid (h,w)) mines minePositions
    where minePositions = cartesian [0..(h-1)] [0..(w-1)]

makeGridMines :: StdGen -> Grid -> Int -> [(Int,Int)] -> Grid
makeGridMines _ grid 0 _  = grid
makeGridMines _ grid _ [] = error "makeGrid: positionList too short."
makeGridMines g grid m mp = update grid' (row, col, Just Mine, Nothing)
    where
        (i, g') = randomR (0, length mp-1) g
        ((row,col), mp', _, _) = pop i mp
        grid' = makeGridMines g' grid (m-1) mp'

makeGridNumerics :: Grid -> Grid
makeGridNumerics grid = foldr makeGridNumeric grid nonMines 
    where
        (h,w) = size grid
        cells = cartesian [0..(h-1)] [0..(w-1)] `zip` concat (rows grid) 
        nonMines = map fst $filter ((/=) Mine . content . snd) cells

makeGridNumeric :: (Int, Int) -> Grid -> Grid
makeGridNumeric (r,c) grid 
    | mines == 0 = grid
    | otherwise  = update grid (r, c, Just (Numeric (fromIntegral mines)), Nothing)
    where mines = length $filter ((==) Mine . content) $getSurrounding (r,c) grid

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
        cont' = fromMaybe (content cell) cont
        stat' = fromMaybe (status cell) stat
        (r, _, rs1, rs2) = pop row (rows grid)
        rows' = rs1 ++ [r !!= (col, Cell cont' stat')] ++ rs2

setCell :: Grid -> (Int, Int) -> Status -> Maybe Grid
setCell grid (row, col) stat
    | status  cell == Open = Nothing
    | status  cell == stat = Nothing
    | otherwise = Just $ update grid (row, col, Nothing, Just stat)
    where cell = rows grid !! row !! col

getSurrounding :: (Int,Int) -> Grid -> [Cell]
getSurrounding (row,col) = concatMap (take3 col) . take3 row . rows




