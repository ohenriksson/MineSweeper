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

newtype Interface = Interface
  { iEmptyGrid  :: (Int,Int) -> Grid
  , iIsLost     :: Grid -> Bool
  , iIsAllOpen  :: Grid -> Bool
  }

-- | start gameLoop on user input
-- runGame :: Interface -> IO ()
-- TODO


-- | game loop, let user set up a new game and playMatch or exit.
-- gameLoop :: Interface -> IO ()
-- TODO


-- | play one match, interact with user,
-- playMatch :: Minefield -> IO()
-- TODO
