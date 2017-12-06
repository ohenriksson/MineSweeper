import Minefield
import Test.QuickCheck

prop_emptyGrid_size :: (Int,Int) -> Property
prop_emptyGrid_size (w,h) = 
    w >= 0 && h >= 0 ==>
        length (rows grid) == h
        && and [length r == w | r <- rows grid]
        && size grid == (w,h)
    where grid = emptyGrid (w,h)

prop_emptyGrid_allClosedEmpty :: (Int,Int) -> Property
prop_emptyGrid_allClosedEmpty (w,h) =
    w >= 0 && h >= 0 ==>
        and [c == Cell Empty Closed | c <- concat $ rows grid]
    where grid = emptyGrid (w,h)

-- | Test correct replacement in list
prop_replaceInList_correct :: [Int] -> [Int] -> Int -> Int -> Property
prop_replaceInList_correct l1 l2 old new = 
    not (null l2) && old /= new ==> 
        l1 ++ [new] ++ l2 == l !!= (length l1, new)
    where l = (l1 ++ [new] ++ l2)

-- | Test correct update in grid
-- TODO prop_update_correct :: Grid ->