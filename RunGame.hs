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

module RunGame where

import Minefield
import System.Random
import Text.Read
import Data.Char
import Data.Maybe
import Data.List.Split


-- | start gameLoop on user input
runGame ::  IO ()
runGame = do
  putStrLn "---Minesweeper---"
  gameLoop

-- | game loop, let user set up a new game and playMatch or exit.
gameLoop :: IO ()
gameLoop = do
  putStrLn "match 1, enter field size:"
  f <- readInt
  putStrLn "match 1, enter number of mines:"
  m <- readInt
  g <- newStdGen
  let grid = makeGrid g (f,f) m
  playMatch grid
  return ()

-- | read user input
readInt :: IO Int
readInt = do
  s <- getLine
  let mines = read s :: Int
  return mines


-- | play one match, interact with user,
playMatch :: Grid -> IO()
playMatch g | isLost g = putStrLn "you lost!"
            | isAllOpen g = putStrLn "you win!"
            | otherwise = do
  print g
  putStrLn "open: o x y, toggle flag: f x y"
  s <- getLine
  let s' = splitOn " " s
  let action = head (head s')
  let x = read (s'!!1) :: Int
  let y = read (s'!!2) :: Int
  let g' = performAction action (x-1,y-1) g
  print g'
  playMatch g'

-- | perform an action on the minefield and return it
performAction :: Char -> (Int,Int) -> Grid -> Grid
performAction 'o' (x,y) g = fromJust (openCell (x,y) g)
performAction 'f' (x,y) g = fromJust (flagCell (x,y) g)
