import Minefield
import ListFunctions
import Test.QuickCheck
import Data.Maybe
import System.Random

instance Arbitrary StdGen where
    arbitrary = do n <- arbitrary
                   return (mkStdGen n)

instance Arbitrary Cell where
    arbitrary = do 
        content <- arbitrary
        return (Cell content Closed)

instance Arbitrary Content where
    arbitrary = frequency [
        (1, return Mine)
        , (9, return Empty)
        ]

-- | an instance for generating Arbitrary Grid
instance Arbitrary Grid where
    arbitrary = do
        height <- choose (2,50)
        width <- choose (2,50)
        rows <- vectorOf height (vectorOf width arbitrary)
        let mines = count isMine (concat rows)
        let grid = Grid rows (height, width)
        return $ makeGridNumerics grid 

-- | Test correct size of arbitrary Grid
prop_arbitrary_grid_size :: Grid -> Bool
prop_arbitrary_grid_size grid =
    ((==) height . length . rows) grid
    && (all ((==) width . length) . rows) grid
    where (height, width) = size grid

prop_emptyGrid_allClosedEmpty :: (Int,Int) -> Property
prop_emptyGrid_allClosedEmpty (w,h) = w >= 0 && h >= 0 ==>
    and [c == Cell Empty Closed | c <- concat $ rows grid]
    where grid = emptyGrid (w,h)

prop_emptyGrid_size :: (Int,Int) -> Property
prop_emptyGrid_size (h,w) = w > 0 && h > 0 ==>
    length (rows grid) == h
    && and [length r == w | r <- rows grid]
    && size grid == (h,w)
    where grid = emptyGrid (h,w)

prop_emptyGrid_noMines :: (Int,Int) -> Property
prop_emptyGrid_noMines (w,h) = w >= 1 && h >= 1 ==>
    countMines grid == 0
    where grid = emptyGrid (w,h)

-- | Text order of getCells
prop_getCells_order :: Grid -> Bool
prop_getCells_order grid =
    take c (getCells grid) == head (rows grid)
    && drop (c*(r-1)) (getCells grid) == rows grid !! (r-1)
    where (r,c) = size grid

-- | Test size of getCells
prop_getCells_size :: Grid -> Bool
prop_getCells_size grid = uncurry (*) (size grid) == length (getCells grid)

-- | Test size of getSurrounding
prop_getSurrounding_cornerSizes :: Grid -> Property
prop_getSurrounding_cornerSizes grid = w >= 2 && h >= 2 ==>
    all ((==) 4 . length)
    [getSurrounding p grid | p <- cartesian [0,w-1] [0,h-1]]
    where (w,h) = size grid

prop_getSurrounding_edgeSizes :: Grid -> Property
prop_getSurrounding_edgeSizes grid = w >= 3 && h >= 3 ==>
    all ((==) 6 . length)
    [getSurrounding (abs r, abs c) grid | (r,c) <- ps]
    where
        (h,w) = size grid 
        ps = map (uncurry sumTuple)
             $cartesian [(1,1),(1,2-w),(2-h,1),(2-h,2-w)] [(0,-1),(-1,0)]

prop_getSurrounding_otherSizes :: Grid -> Property
prop_getSurrounding_otherSizes grid = w >= 4 && h >= 4 ==>
    all ((==) 9 . length)
    [getSurrounding (r,c) grid
    | (r,c) <- [(1,1), (1,w-2), (h-2,1), (h-2,w-2), (h`div`2,w`div`2)]]
    where (h,w) = size grid 

prop_isAllOpen_truePositive :: Grid -> Bool
prop_isAllOpen_truePositive grid =
    isAllOpen $fromJust $openCells nonMines grid
    where nonMines = positions isNotMine grid

prop_isAllOpen_trueNegative :: Grid -> Int -> Property
prop_isAllOpen_trueNegative grid n =
    n >= 0 && n < h*w - countMines grid ==>
        not $isAllOpen $fromJust $openCells nonMines grid
    where 
        (h,w) = size grid
        (_,nonMines,_,_) = pop n $positions isNotMine grid

prop_isLost_truePositive :: Grid -> Int -> Property
prop_isLost_truePositive grid n =
    n >= 0 && n < countMines grid ==>
        isLost $fromJust $openCell (mines !! n) grid
    where
        (h,w) = size grid
        mines = positions isMine grid

prop_isLost_trueNegative :: Grid -> Bool
prop_isLost_trueNegative grid =
    not $isLost $fromJust $openCells nonMines grid
    where nonMines = positions isNotMine grid

prop_makeGrid_mines :: StdGen -> (Int,Int) -> Int -> Property
prop_makeGrid_mines g (h,w) n = 
    all (0 < ) [w,h,n] &&  h*w > n ==>
        n == countMines grid
    where grid = makeGrid g (h,w) n

prop_makeGrid_numerics :: StdGen -> (Int,Int) -> Int -> Property
prop_makeGrid_numerics g (h,w) n = 
    all (0 < ) [w,h,n] &&  h*w > n ==>
        and [0 == count isMine e | e <- empties]
        && and [fromNumeric ((fst . getCell p) grid) == count isMine e
             | (p,e) <- numerics]
    where 
        grid = makeGrid g (h,w) n
        empties = [getSurrounding p grid | p <- positions isEmpty grid]
        numerics = [(p,getSurrounding p grid) | p <- positions isNumeric grid]

prop_makeGrid_size :: StdGen -> (Int,Int) -> Int -> Property
prop_makeGrid_size g (h,w) n = 
    all (0 < ) [w,h,n] &&  h*w > n ==>
        (h,w) == size grid
        && h == length (rows grid)
        && all (w ==) (map length (rows grid))
    where grid = makeGrid g (h,w) n
