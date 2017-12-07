
import ListFunctions
import Test.QuickCheck
-- | Test correct replacement in list
prop_replaceInList_correct :: [Int] -> [Int] -> Int -> Int -> Property
prop_replaceInList_correct l1 l2 old new = 
    not (null l2) && old /= new ==> 
        l1 ++ [new] ++ l2 == l !!= (length l1, new)
    where l = (l1 ++ [new] ++ l2)