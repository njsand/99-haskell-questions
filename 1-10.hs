-- Solutions to https://www.haskell.org/haskellwiki/99_questions

-- Lists

-- p1
-- last element of a list
mylast :: [a] -> a
mylast (x:[]) = x
mylast (x:xs) = mylast xs

-- Another way
myLast2 = foldl1 (\_ y -> y)

-- p2
-- last but one element
my2ndlast :: [a] -> a
my2ndlast (x:y:[]) = x
my2ndlast (x:xs) = my2ndlast xs

-- p3
-- kth element of a list (1-based)
myElementAt (x:xs) 1 = x
myElementAt (x:xs) n = myElementAt xs (n - 1)

-- p4
-- length
myLen :: [a] -> Int
myLen [] = 0
myLen (x:xs) = 1 + (myLen xs)

-- accumulator edition
myLen2 :: [a] -> Int
myLen2 xs = rec 0 xs
  where rec n [] = n
        rec n (x:xs) = rec (1+n) xs

-- p5
-- reverse
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

-- tail recursive.  faster.
myReverseAcc :: [a] -> [a]
myReverseAcc xs = rec xs []
  where rec [] acc = acc
        rec (x:xs) acc = rec xs (x:acc)

-- p6
palindrome x = x == myReverse x

-- p7
-- flatten a list
data NestedList a = Elem a | List [NestedList a] deriving (Show)

flat2 [] = []
flat2 (x:xs) = flatten x ++ flat2 xs

flatten :: NestedList a -> [a]
flatten (Elem a) = [a]
-- flatten (List []) = []
flatten (List xs) = flat2 xs

-- p8
-- replace dups with a single element
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = rec x xs
  where rec x [] = [x]
        rec x (y:ys)
          | x == y = rec x ys
          | otherwise = x:(rec y ys)

compress2 :: (Eq a) => [a] -> [a]
compress2 [] = []
compress2 [x] = [x]
compress2 (x:xs) =
  let rest@(r:rs) = compress2 xs in
  if r == x then rest else x:rest

-- p9
-- Group adjacent equal items into their own lists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = rec [x] xs
  where rec xs [] = [xs]
        rec xs@(x:_) (y:ys)
          | y == x = rec (y:xs) ys
          | otherwise = xs:(rec [y] ys)

-- p10
-- Run length encoding.
rle :: (Eq a) => [a] -> [(Int, a)]
rle [] = []
rle xs = map (\x -> (length x, head x)) $ pack xs
