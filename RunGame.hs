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


-- | start gameLoop on user input
runGame ::  IO ()
runGame = do
  putStrLn "---Minesweeper---"
  gameLoop

-- | game loop, let user set up a new game and playMatch or exit.
gameLoop :: IO ()
gameLoop = do
  putStrLn "match 1:"
  playMatch g
  where g = emptyGrid (4,4)

-- | play one match, interact with user,
playMatch :: Grid -> IO()
playMatch mField = putStrLn (show mField)
