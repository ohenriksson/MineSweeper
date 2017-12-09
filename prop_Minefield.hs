import Minefield
import ListFunctions
import Test.QuickCheck
import Data.Maybe

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
        let mines = length (filter ((Mine ==) . content) (concat rows))
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
prop_emptyGrid_size (w,h) = w >= 0 && h >= 0 ==>
    length (rows grid) == h
    && and [length r == w | r <- rows grid]
    && size grid == (w,h)
    where grid = emptyGrid (w,h)

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

prop_isAllOpen_truePositive :: Grid -> Property
prop_isAllOpen_truePositive grid = w >= 1 && h >= 1 ==>
    isAllOpen $fromJust $openCells nonMines grid
    where
        (h,w) = size grid
        nonMines = map snd . filter (isNotMine . fst)
                   $ getCells grid `zip` cartesian [0..h-1] [0..w-1]

prop_isAllOpen_trueNegative :: Grid -> Int -> Property
prop_isAllOpen_trueNegative grid n =
    w >= 1 && h >= 1 && n >= 0 && n < countMines grid - 1 ==>
        not $isAllOpen $fromJust $openCells nonMines' grid
    where
        (h,w) = size grid
        nonMines  = map snd . filter (isNotMine . fst)
                    $ getCells grid `zip` cartesian [0..h-1] [0..w-1]
        (_,nonMines',_,_) = pop n nonMines



-- | Test correct update in grid
-- TODO prop_update_correct :: Grid ->

-- | Test test setCell
-- TODO 

-- | Test nonMines

-- | Test isLostCell

-- | Test isLost

-- | Test isOpen

-- | Test isAllOpen