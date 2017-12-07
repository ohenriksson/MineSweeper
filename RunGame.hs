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

data Interface = Interface
  { iEmptyGrid  :: (Int,Int) -> Grid
  , iIsLost     :: Grid -> Bool
  , iIsAllOpen  :: Grid -> Bool
  }

-- | start gameLoop on user input
runGame :: Interface -> IO ()
runGame iFace = do
  putStrLn "---Minesweeper---"
  gameLoop iFace

-- | game loop, let user set up a new game and playMatch or exit.
gameLoop :: Interface -> IO ()
gameLoop iFace = do
  putStrLn "match 1:"
  -- TODO

-- | play one match, interact with user,
-- playMatch :: Minefield -> IO()
-- TODO
