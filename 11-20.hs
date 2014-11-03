
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

-- kindof cheaty:
mymax (x:xs) = mymax' xs x
  where mymax' [] max = max
        mymax' (x:xs) max
          | x > max = mymax' xs x
          | otherwise = mymax' xs max

mymax2 [x] = x
mymax2 (x:xs) =
  let max = mymax2 xs
  in if max > x then max else x

data Light = Red | Green | Orange

instance Show Light where
  show Red = "red light"
  show Green = "green light"
  show Orange = "orange light"

test x = case x of
  1 -> "one"
  2 -> "two"
  3 -> "what"

-- test 1 = "one"
-- test 2 = "two"
-- test 3 = "what"
