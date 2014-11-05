-- Problems 21 to 30.

-- p21
-- Insert an item at a given position in a list.
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = let (front, back) = splitAt (n - 1) xs in front ++ (x:back)

-- p22
-- Create a list of integers in a given range.  Easy!
range :: Int -> Int -> [Int]
range x y = [x..y]

-- p23
-- Extract a given number of randomly selected elements from a list. 
-- 
