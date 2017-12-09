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
data Grid = Grid { rows :: [[Cell]], size :: (Int,Int)}
            deriving (Eq)

-- | Ascii representation of minefield
instance Show Grid where
    show grid = showGridTop grid ++ showGridRows grid

-- | Make size strings of increamenting numbers, each string of 3 characters 
showGridBar :: Int -> [String]
showGridBar size = map showGridTick [1..size]

showGridTick :: Int -> String 
showGridTick n 
    | n < 10    = " " ++ show n ++ " "
    | n < 100   = " " ++ show n
    | otherwise = show n

showGridRow :: Int -> [Cell] -> String
showGridRow n cells = showGridTick n ++ concatMap show cells ++ "\n"

showGridRows :: Grid -> String
showGridRows grid = (concatMap (uncurry showGridRow) . zip [1..n] . rows) grid 
    where (n, _) = size grid

showGridTop :: Grid -> String
showGridTop grid =  "   " ++ (concat . showGridBar . snd . size) grid ++ "\n"

-- | Given a size (width, height), create a grid with no mines
emptyGrid :: (Int, Int) -> Grid
emptyGrid (h, w) = Grid rows (h, w)
   where rows = replicate h $ replicate w $ Cell Empty Open

getCell :: (Int,Int) -> Grid -> (Content,Status)
getCell (r,c) grid = (content cell, status cell) 
    where cell = rows grid !! r !! c 

-- | Given a Grid, get a list of all Cells
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
makeGridMines mp = updateContent Mine (row, col) . makeGridMines mp' 
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
    | otherwise  = updateContent (Numeric mines) (r,c) grid
    where
        surrondingContent = map content $getSurrounding (r,c) grid
        mines = fromIntegral $count Mine surrondingContent


updateContent :: Content -> (Int, Int) -> Grid -> Grid
updateContent co' (r,c) grid =
    Grid (rows grid !!!= (r, c, Cell co' st)) $size grid
    where (_,st) = getCell (r,c) grid

updateStatus :: Status -> (Int, Int) -> Grid -> Maybe Grid
updateStatus st' (r,c) grid
    | st == Open  = Nothing
    | st == st'   = Nothing
    | otherwise   = Just $Grid (rows grid !!!= (r, c, Cell co st')) $size grid
    where (co,st) = getCell (r,c) grid

openCell :: (Int, Int) -> Grid -> Maybe Grid
openCell =  updateStatus Open

flagCell :: (Int, Int) -> Grid -> Maybe Grid
flagCell =  updateStatus Flagged

unflagCell :: (Int, Int) -> Grid -> Maybe Grid
unflagCell =  updateStatus Closed