import Minefield
import Test.QuickCheck

prop_emptyGrid_allClosedEmpty :: (Int,Int) -> Property
prop_emptyGrid_allClosedEmpty (w,h) =
    w >= 0 && h >= 0 ==>
        and [c == Cell Empty Closed | c <- concat $ rows grid]
    where grid = emptyGrid (w,h)

prop_emptyGrid_size :: (Int,Int) -> Property
prop_emptyGrid_size (w,h) = 
    w >= 0 && h >= 0 ==>
        length (rows grid) == h
        && and [length r == w | r <- rows grid]
        && size grid == (w,h)
    where grid = emptyGrid (w,h)


-- | Test correct update in grid
-- TODO prop_update_correct :: Grid ->

-- | Test test setCell
-- TODO 

-- | Test nonMines

-- | Test isLostCell

-- | Test isLost

-- | Test isOpen

-- | Test isAllOpen