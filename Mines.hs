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

module Mines where

-- | TODO Commenting
data Cell = Cell { content :: CellContent, status :: CellStatus }
            deriving (Eq, Show)

data CellContent = Numeric Integer | Mine | Empty
                   deriving (Eq, Show)

data CellStatus = Open | Closed | Flaged
                  deriving (Eq, Show)

{-
TODO Implement Arbitrary Cell
-}

-- | TODO Comment
data Minefield = Minefield { rows :: [[Cell]], size :: (Int,Int)}
             deriving (Eq, Show)

{-
TODO Implement Arbitrary Board
-}
