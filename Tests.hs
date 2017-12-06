import Minesweeper
import Mines
import Test.QuickCheck

prop_makeEmptyMinefield_size :: (Int,Int) -> Property
prop_makeEmptyMinefield_size (w,h) = 
    w >= 0 && h >= 0 ==>
        length (rows mf) == h
        && and [length r == w | r <- rows mf]
        && size mf == (w,h)
    where mf = makeEmptyMinefield (w,h)

prop_makeEmptyMinefield_allClosedEmpty :: (Int,Int) -> Property
prop_makeEmptyMinefield_allClosedEmpty (w,h) =
    w >= 0 && h >= 0 ==>
        and [c == Cell Empty Closed | c <- concat $ rows mf]
    where mf = makeEmptyMinefield (w,h)

--prop_updateContent_