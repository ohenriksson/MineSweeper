
import ListFunctions
import Test.QuickCheck

-- | test (!!=) for correct insertion
prop_insert_1d :: (Eq a) => [a] -> (Int,a) -> Property
prop_insert_1d l (i,new) =
    i `elem` [0..length l-1] ==>
    l'!!i == new
    && length l == length l'
    where
      l' = l !!= (i,new)

-- | test (!!!=) for correct insertion
prop_insert_2d :: (Eq a) => [[a]] -> (Int,Int,a) ->  Property
prop_insert_2d l (r,c,new) =
  r `elem` [0..length l-1]
  && c `elem` [0..length row-1] ==>
  (l'!!r)!!c == new
  && length l == length l'
  where row = l!!r
        l' = l !!!= (r,c,new)
--TODO: quickcheck gives up


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
        (x', l', l1', l2') = pop i l
