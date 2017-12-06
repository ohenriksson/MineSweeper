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

import Mines

makeEmptyMinefield :: (Int, Int) -> Minefield
makeEmptyMinefield (w, h) = Minefield rows (w,h)
    where rows = replicate h $ replicate w $ Cell Empty Closed
