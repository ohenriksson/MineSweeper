module ListFunctions where

inRange :: (Eq a, Ord a) => a -> a -> a -> Bool
inRange a b c = a <= b && b < c

-- | For a given list l, and a given tuple (i, v),
--   replace element i in l with v
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, r)
    | null l                     = error "(!!): List is empty."
    | not$inRange 0 i (length l) = error "(!!): Index out of range."
    | otherwise                  = l1 ++ [r] ++ l2
    where (_, _, l1, l2) = pop i l

pop :: Int -> [a] -> (a, [a], [a], [a])
pop i l 
    | not$inRange 0 i (length l - 1) = error "pop: Index out of range."
    | otherwise = (head l2, l1 ++ (drop 1 l2), l1, drop 1 l2)
    where (l1, l2) = splitAt i l
