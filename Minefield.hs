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
   where rows = replicate h $ replicate w $ Cell Empty Open

getCells :: Grid -> [Cell] 
getCells = concat . rows

-- | Given coordinates p and a grid, return surrounding cell incl cell on p
getSurrounding :: (Int,Int) -> Grid -> [Cell]
getSurrounding (row,col) = concatMap (take3 col) . take3 row . rows

-- | Test if all non-mines has been open
isAllOpen :: Grid -> Bool
isAllOpen = all ((==) Open . status) . filter ((/=) Mine . content) . getCells

-- | Test if any mine has been open
isLost :: Grid -> Bool
isLost = any ((==) Open . status) . filter ((==) Mine . content) . getCells

-- | Given an StdGen, a size, and a number of mines, make a random Minefield
makeGrid :: StdGen -> (Int, Int) -> Int -> Grid
makeGrid g (h, w) n
    | any (0 >) [w,h,n] = error "makeGrid: Negative numbers forbidden."
    | (w*h) <= n        = error "makeGrid: Too many mines."
    | otherwise = makeGridNumerics
                  $makeGridMines mines (emptyGrid (h,w))
    where mines = takeRandom g n $cartesian [0..(h-1)] [0..(w-1)]

makeGridMines :: [(Int,Int)] -> Grid -> Grid
makeGridMines [] = nop
makeGridMines mp = update (row, col) (Just Mine, Nothing) . makeGridMines mp' 
    where ((row,col), mp', _, _) = pop 0 mp

makeGridNumerics :: Grid -> Grid
makeGridNumerics grid = foldr makeGridNumeric grid nonMines 
    where
        (h,w) = size grid
        cells = cartesian [0..(h-1)] [0..(w-1)] `zip` getCells grid 
        nonMines = map fst $filter ((/=) Mine . content . snd) cells

makeGridNumeric :: (Int, Int) -> Grid -> Grid
makeGridNumeric (r,c) grid 
    | mines == 0 = grid
    | otherwise  = update (r, c) (Just $Numeric mines, Nothing) grid
    where
        surrondingContent = map content $getSurrounding (r,c) grid
        mines = fromIntegral $count Mine surrondingContent

-- | For a given Grid grid, and a given tuple (row, col, cont, stat),
--   update cell (row, col) with non-nothing cont and stat
update :: (Int, Int) -> (Maybe Content, Maybe Status) -> Grid -> Grid
update _ (Nothing, Nothing) grid = grid
update (row, col) (cont, stat) grid
    | not$inRange 0 row $fst(size grid) = error "update: Row out of range."
    | not$inRange 0 col $snd(size grid) = error "update: Column out of range."
    | otherwise = Grid rows' (size grid) (mines grid)
    where
        cell  = rows grid !! row !! col
        cont' = fromMaybe (content cell) cont
        stat' = fromMaybe (status cell) stat
        (r, _, rs1, rs2) = pop row (rows grid)
        rows' = rs1 ++ [r !!= (col, Cell cont' stat')] ++ rs2

setCellStatus :: Status -> (Int, Int) -> Grid -> Maybe Grid
setCellStatus stat (row, col) grid
    | status  cell == Open = Nothing
    | status  cell == stat = Nothing
    | otherwise = Just $ update (row, col) (Nothing, Just stat) grid
    where cell = rows grid !! row !! col

openCell :: (Int, Int) -> Grid -> Maybe Grid
openCell =  setCellStatus Open

flagCell :: (Int, Int) -> Grid -> Maybe Grid
flagCell =  setCellStatus Flagged

unflagCell :: (Int, Int) -> Grid -> Maybe Grid
unflagCell =  setCellStatus Closed