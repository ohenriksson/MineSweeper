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
    show (Cell  _          Flaged) = "[f]"
    show (Cell  Empty      Open)   = "   "
    show (Cell  Mine       Open)   = "}#{"
    show (Cell (Numeric n) Open)   = " " ++ show n ++ " "

-- | Representation of whether a Mine is in cell or close
data Content = Numeric Integer | Mine | Empty
               deriving (Eq)

-- | Representation of whether cell is open, closed or flaged by player
data Status = Open | Closed | Flaged
              deriving (Eq)

-- | Representation of a Minefield
data Grid = Grid { rows :: [[Cell]], size :: (Int,Int), mines :: Int}
            deriving (Eq)
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
makeGrid g (w, h) mines
    | any (0 <) [w,h,mines] = error "makeGrid: Negative numbers forbidden."
    | (w*h) <= mines          = error "makeGrid: Too many mines."
    | otherwise = makeGrid' g (emptyGrid (w,h)) mines minePositions
    where minePositions = [(r,c) | r <- [0..(h-1)], c <- [0..(w-1)]]

makeGrid' :: StdGen -> Grid -> Int -> [(Int,Int)] -> Grid
makeGrid' _ grid 0 _  = grid
makeGrid' _ grid _ [] = error "makeGrid: positionList too short."
makeGrid' g grid m mp = update grid' (row, col, Just Mine, Nothing)
    where
        (i, g') = randomR (0, length mp) g
        ((row,col), mp', _, _) = pop mp i
        grid' = makeGrid' g' grid (m-1) mp'

printGrid :: Grid -> IO ()
printGrid grid = do
    let (w, h) = size grid
    putStrLn "Heyy"

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
        (r, _, rs1, rs2) = pop (rows grid) row
        rows' = rs1 ++ [r !!= (col, Cell cont' stat')] ++ rs2

setCell :: Grid -> (Int, Int) -> Content -> Maybe Grid
setCell grid (row, col) cont
        | status  cell == Open = Nothing
        | content cell == cont = Nothing
        | otherwise = Just $ update grid (row, col, Just cont, Nothing)
    where cell  = rows grid !! row !! col
