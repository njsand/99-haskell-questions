import System.Random
import Control.Monad(replicateM)

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
  | y == x2 + 1 = (x1, y):xs
  | y > x2 = range : (addStep xs y)

inRange :: (Ord a) => (a, a) -> a -> Bool
inRange (x, y) z = z >= x && z <= y

index (Ord a, Num a) => [(a, a)] -> Int -> a
index [] x = x
index []
