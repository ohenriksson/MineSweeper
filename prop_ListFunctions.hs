
import ListFunctions
import Test.QuickCheck

-- | Test correct replacement in list
prop_replaceInList :: [Int] -> [Int] -> Int -> Int -> Property
prop_replaceInList l1 l2 old new = 
    not (null l2) && old /= new ==> 
        l1 ++ [new] ++ l2 == l !!= (length l1, new)
    where l = l1 ++ [new] ++ l2

-- | Test inRange, some basic cases
prop_inRange :: Int -> Bool
prop_inRange x =
    not $inRange x x x
    && not (inRange (x-1) x x)
    && not (inRange (x+1) x x)
    && not (inRange x x (x-1))
    && inRange x x (x+1)
    && not (inRange (x-1) x (x-1))
    && inRange (x-1) x (x+1)
    && not (inRange (x+1) x (x-1))
    && not (inRange (x+1) x (x+1))

-- | Test correct values from pop
prop_pop :: [Int] -> [Int] -> Int -> Property
prop_pop l1 l2 x = 
    not (null l2) ==>
        x' == x
        && l1' == l1
        && l2' == l2
        && l' == l1 ++ l2
    where
        i = length l1
        l = l1 ++ [x] ++ l2
        (x', l', l1', l2') = pop l i