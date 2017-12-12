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

-- Should export only countMines, isAllOpen, isLost, makeGrid,
-- openCell, flagCell, unflagCell 

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
data Content = Numeric Int | Mine | Empty
               deriving (Eq)

fromNumeric :: Content -> Int
fromNumeric (Numeric n) = n
fromNumeric _ = error "fromNumeric: Not numeric"

-- | Representation of whether cell is open, closed or flaged by player
data Status = Open | Closed | Flagged
              deriving (Eq)

-- | Representation of a Minefield
data Grid = Grid { rows :: [[Cell]], size :: (Int,Int)}
            deriving (Eq)

-- | Ascii representation of minefield
instance Show Grid where
    show grid = showGridTop grid ++ showGridRows grid

-- | Help functions to show grid.
showGridBar :: Int -> [String]
showGridBar size = map showGridTick [1..size]
showGridRow :: Int -> [Cell] -> String
showGridRow n cells = showGridTick n ++ concatMap show cells ++ "\n"
showGridRows :: Grid -> String
showGridRows grid = (concatMap (uncurry showGridRow) . zip [1..n] . rows) grid 
    where (n, _) = size grid
showGridTick :: Int -> String 
showGridTick n 
    | n < 10    = " " ++ show n ++ " "
    | n < 100   = " " ++ show n
    | otherwise = show n
showGridTop :: Grid -> String
showGridTop grid =  "   " ++ (concat . showGridBar . snd . size) grid ++ "\n"

-- | Given a grid, return number of mines it contains.
countMines :: Grid -> Int
countMines = count isMine . getCells

-- | Given a size (width, height), create a grid with no mines.
emptyGrid :: (Int, Int) -> Grid
emptyGrid (h, w) = Grid rows (h, w)
   where rows = replicate h $ replicate w $ Cell Empty Closed

flagCell :: (Int, Int) -> Grid -> Maybe Grid
flagCell = updateStatus Flagged

-- | Given a coordinate and a grid, return content and status of cell
getCell :: (Int,Int) -> Grid -> (Content,Status)
getCell (r,c) grid = (content cell, status cell) 
    where cell = rows grid !! r !! c 

-- | Given a Grid, get a list of all Cells.
getCells :: Grid -> [Cell] 
getCells = concat . rows

-- | Given coordinates p and a grid, return surrounding cell incl cell on p
getSurrounding :: (Int,Int) -> Grid -> [Cell]
getSurrounding (row,col) = concatMap (take3 col) . take3 row . rows

-- | Test if all non-mines has been open
isAllOpen :: Grid -> Bool
isAllOpen = all isOpen . filter isNotMine . getCells

isEmpty :: Cell -> Bool
isEmpty = (==) Empty . content

isNumeric :: Cell -> Bool
isNumeric cell = not $ isEmpty cell || isMine cell

-- | Test if any mine has been open
isLost :: Grid -> Bool
isLost = any isOpen . filter isMine . getCells

isMine :: Cell -> Bool
isMine = (==) Mine . content

isNotMine :: Cell -> Bool
isNotMine = not . isMine

isOpen :: Cell -> Bool
isOpen = (==) Open . status

-- | Given an StdGen, a size, and a number of mines, make a random Minefield
makeGrid :: StdGen -> (Int, Int) -> Int -> Grid
makeGrid g (h, w) n
    | any (0 >) [w,h,n] = error "makeGrid: Negative orbidden."
    | (w*h) <= n        = error "makeGrid: Too many mines."
    | otherwise = makeGridNumerics
                  $makeGridMines mines (emptyGrid (h,w))
    where mines = takeRandom g n $cartesian [0..(h-1)] [0..(w-1)]

makeGridMines :: [(Int,Int)] -> Grid -> Grid
makeGridMines [] = nop
makeGridMines mp = updateContent Mine (row, col) . makeGridMines mp' 
    where ((row,col), mp', _, _) = pop 0 mp

makeGridNumerics :: Grid -> Grid
makeGridNumerics grid = foldr makeGridNumeric grid nonMines 
    where nonMines = positions isNotMine grid

makeGridNumeric :: (Int, Int) -> Grid -> Grid
makeGridNumeric (r,c) grid 
    | mines == 0 = grid
    | otherwise  = updateContent (Numeric mines) (r,c) grid
    where
        surronding = getSurrounding (r,c) grid
        mines = count isMine surronding

openCell :: (Int, Int) -> Grid -> Maybe Grid
openCell (r,c) grid
    | cell == (Empty,Closed) = Just $foldr tryOpenCell (fromJust grid')
                                $cartesian [r-1..r+1] [c-1..c+1]
    | otherwise = grid'
    where
        cell = getCell (r,c) grid
        grid' = updateStatus Open (r,c) grid

openCells :: [(Int,Int)] -> Grid -> Maybe Grid
openCells l grid 
    | null l        = Just grid
    | grid == grid' = Nothing
    | otherwise     = Just grid'
    where grid' = foldr tryOpenCell grid l

tryOpenCell :: (Int, Int) -> Grid -> Grid
tryOpenCell (r,c) grid 
    | r<0 || c<0 || r>=h || c>=w = grid
    | otherwise = fromMaybe grid $openCell (r,c) grid
    where (h,w) = size grid

positions :: (Cell -> Bool) -> Grid -> [(Int,Int)]
positions f grid = map snd . filter (f . fst) $ getCells grid `zip` allPos
    where allPos = cartesian [0..fst (size grid)-1] [0.. snd (size grid)-1]

unflagCell :: (Int, Int) -> Grid -> Maybe Grid
unflagCell = updateStatus Closed

updateContent :: Content -> (Int, Int) -> Grid -> Grid
updateContent co' (r,c) grid =
    Grid (rows grid !!!= (r, c, Cell co' st)) $size grid
    where (_,st) = getCell (r,c) grid

updateStatus :: Status -> (Int, Int) -> Grid -> Maybe Grid
updateStatus st' (r,c) grid
    | st == Open                   = Nothing
    | st == st'                    = Nothing
    | st == Flagged && st' == Open = Nothing
    | otherwise   = Just $Grid (rows grid !!!= (r, c, Cell co st')) $size grid
    where (co,st) = getCell (r,c) grid
