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

module MineField where

{-
TODO Implement Cell
Should contain Mine or an integer 0-8
May be open, closed or flaged
-}
data Cell = Cell { content :: CellContent, status :: CellStatus }
            deriving (Eq, Show)

data CellContent = Numeric Integer | Mine 
                   deriving (Eq, Show)

data CellStatus = Open | Closed | Flaged
                  deriving (Eq, Show)

{-
TODO Implement Arbitrary Cell
-}

{-
TODO Implement Board
Should contain 2 dimensional grid of cells. Make it two lists
-}

{-
TODO Implement Arbitrary Board
-}
