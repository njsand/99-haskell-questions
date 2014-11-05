-- Problems 11 to 20.

-- p11
-- This is RLE again, but don't put single items in their own tuple, just put 
-- them "by themselves".  We need a new data type (lists are homogenous).
data Rle a = Mult Int a | Single a deriving (Eq, Ord, Show, Read)

rle2 :: (Eq a) => [a] -> [Rle a]
rle2 [] = []
rle2 (x:xs) = rec (Single x) xs
  where rec x [] = [x]
        rec s@(Single x) (y:ys)
          | x == y = rec (Mult 2 x) ys
          | otherwise = s : rec (Single y) ys
        rec m@(Mult n x) (y:ys)
          | x == y = rec (Mult (1 + n) x) ys
          | otherwise = m : rec (Single y) ys

-- p12
-- decode a run-length encoded list.
drle :: [Rle a] -> [a]
drle [] = []
drle ((Single x):xs) = x : drle xs
drle ((Mult n x):xs) = (take n $ repeat x) ++ drle xs

-- p13
-- not much point to this one

-- p14
-- duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

-- p15
-- replicate the elements of a list a given number of times
repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ repli xs n

-- p16
-- drop every nth element from a list
dropNth :: Int -> [a] -> [a]
dropNth _ [] = []
dropNth n xs = map snd $ filter (\x -> fst x `mod` n /= 0) $ zip [1..] xs

-- p17
-- Split a list into 2 parts.  The length of the first part is given.
-- (Should be O(n).)
split :: [a] -> Int -> [[a]]
split xs n = rec [] xs n
  where rec xs ys 0 = [reverse xs, ys]
        rec xs [] _ = [xs, []]
        rec xs (y:ys) n = rec (y:xs) ys (n - 1)

-- Can also use the built-in: splitAt
split2 :: [a] -> Int -> [[a]]
split2 xs n = let (front, back) = splitAt n xs in [front, back]

-- p18
-- Return a slice of a list. 1-based.  Positions outside the list bounds don't
-- produce errors.
slice :: [a] -> Int -> Int -> [a]
slice xs i k = take (k - i + 1) $ drop (i - 1) xs

-- p19
-- Rotate a list N places to the left.  (N may be negative and is a rotation to the right).
rotate :: [a] -> Int -> [a]
rotate xs n = drop rot xs ++ take rot xs
  where rot = n `mod` length xs

-- p20
-- Remove the k'th item from a list.  (k starts at 1).
removeAt :: [a] -> Int -> (a, [a])
removeAt xs k = let (front, (b:back)) = splitAt (k - 1) xs in (b, front ++ back)

