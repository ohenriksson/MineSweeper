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
import Control.Monad


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
  Control.Monad.when ( isNothing f) gameLoop
  let s = fromJust f
  putStrLn "match 1, enter number of mines:"
  f <- readInt
  Control.Monad.when ( isNothing f) gameLoop
  let mines = fromJust f
  g <- newStdGen
  let grid = makeGrid g (s,s) mines
  playMatch grid
  putStrLn "play again? (y/n)"
  s <- getLine
  Control.Monad.when ( head s == 'y') gameLoop

-- | read user input
readInt :: IO (Maybe Int)
readInt = do
  s <- getLine
  let mines = readMaybe s :: Maybe Int
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
  Control.Monad.when (length s' < 3 ) (playMatch g)
  let x = readMaybe (s'!!1) :: Maybe Int
  let y = readMaybe (s'!!2) :: Maybe Int
  Control.Monad.when (isNothing x || isNothing y) (playMatch g)
  let x' = fromJust x
  let y' = fromJust y
  let g' = performAction action (x'-1,y'-1) g
  playMatch g'

-- | perform an action on the minefield and return it
performAction :: Char -> (Int,Int) -> Grid -> Grid
performAction 'o' (x,y) g = fromMaybe g (openCell (x,y) g)
performAction 'f' (x,y) g = fromMaybe g (toggleFlag (x,y) g)
