import System.Random

-- Problems 21 to 30.

-- p21
-- Insert an item at a given position in a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = let (front, back) = splitAt (n - 1) xs in front ++ (x:back)

-- insertAt' :: a -> [a] -> Int -> [a]
-- insertAt' x xs n = rec x 

-- NewType DiffList a = DiffList { getDiffList :: ([a] -> [a]) }

-- instance DiffList Monoid where
--   mempty = DiffList (\x -> x ++ [])
--   mappend x y = DiffList

-- p22
-- Create a list of integers in a given range.  Easy!
range :: Int -> Int -> [Int]
range x y = [x..y]

-- p23
-- Extract a given number of randomly selected elements from a list.
rndSelect :: (RandomGen g) => g -> [a] -> Int -> [a]
rndSelect g xs n = map (xs !!) $ take n $ randomRs (0, length xs - 1) g
