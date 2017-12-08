module ListFunctions where

-- | For a given list l, and a given tuple (i, v),
--   replace element i in l with v
(!!=) :: [a] -> (Int,a) -> [a]
(!!=) l (i, r)
    | null l                     = error "(!!): List is empty."
    | not$inRange 0 i (length l) = error "(!!): Index out of range."
    | otherwise                  = l1 ++ [r] ++ l2
    where (_, _, l1, l2) = pop i l

-- | For two given lists, return the cartesian product
cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys = [(x,y) | x <- xs, y <- ys]

-- | Count occurances of given element in given list
count :: (Eq a) => a -> [a] -> Int
count x l = length $filter (x ==) l

-- | For three given elements, return True if second element
--   is equal to or larger than first, but small than third
inRange :: (Eq a, Ord a) => a -> a -> a -> Bool
inRange a b c = a <= b && b < c

-- | For a given list l, and a given index i, remove element i from l
--   and return tuple with <element i>, <new list>, <elements before i>,
--   <elements after i>
pop :: Int -> [a] -> (a, [a], [a], [a])
pop i l
    | not$inRange 0 i (length l) = error "pop: Index out of range."
    | otherwise = (head l2, l1 ++ drop 1 l2, l1, drop 1 l2)
    where (l1, l2) = splitAt i l

take3 :: Int -> [a] -> [a]
take3 i = drop (i - 1) . take (i + 2)