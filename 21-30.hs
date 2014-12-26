import System.Random
import Control.Monad(replicateM)

import Data.List(sortBy)
import Data.Function(on)

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

-- An example of calling this
rndSelectEx :: (Show a) => [a] -> Int -> IO ()
rndSelectEx xs n = do
  gen <- newStdGen
  putStrLn $ show $ rndSelect gen xs n

rndSelect2 :: [a] -> Int -> IO [a]
rndSelect2 xs n = do
  indexes <- replicateM n $ getStdRandom $ randomR (0, length xs - 1)
  return $ map (xs !!) indexes

-- p24
-- Lotto: Draw N different random numbers from the set 1..M. 
-- (So the numbers have to be different.)
-- lotto :: Int -> Int -> [Int]

-- First define a data structure that allows for a (mostly) compact
-- representation of a function that is basically \x -> x but has some
-- discontinuities/steps in it.
addStep :: (Ord a, Num a) => [(a, a)] -> a -> [(a, a)]
addStep [] y = [(y, y)]
addStep all@(range@(x1, x2):xs) y
  | inRange range y = all
  | y == x1 - 1 = (y, x2):xs
  | y < x1 = (y, y):all
  | y == x2 + 1 = merge (x1, y) xs
  | y > x2 = range : (addStep xs y)

merge :: (Ord a, Num a) => (a, a) -> [(a, a)] -> [(a, a)]
merge (low, high) [] = [(low, high)]
merge pair1@(low1, high1) rest@((low2, high2):xs)
  | high1 + 1 == low2 = (low1, high2):xs
  | otherwise = (pair1 : rest)

-- Return true if LOW <= MIDDLE <= HIGH.
inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (low, high) middle = middle >= low && middle <= high

index :: (Ord a, Num a) => [(a, a)] -> a -> a
index [] y = y
index ((x1, x2):xs) y = 
  if y < x1 
  then y
  else index xs (y + (x2 - x1 + 1))

recR :: Int -> Int -> [(Int, Int)] -> IO [Int]
recR 0 max _ = return []
recR n max map = do
  r <- getStdRandom $ randomR (1, max)
  let result = index map r
  rest <- recR (n - 1) (max - 1) $ addStep map result
  return (result:rest)

lotto :: Int -> Int -> IO [Int]
lotto max n
  | n > max = error "Numbers requested must be less than the upper bound"
  | otherwise = recR n max []

-- p25
-- Generate a random permutation of the elements of a list.
rnd_permu :: [a] -> IO [a]
rnd_permu xs = do
  indexes <- lotto (length xs) (length xs)
  return $ (map snd . sortBy (compare `on` fst) . zip indexes) xs

-- p26
-- combinations :: Int -> [a] -> [[a]]
-- combinatins 1 xs = xs
-- combinations n (xs) = do 
--   x <- xs
--   x:(combinations (n - 1) xs)
