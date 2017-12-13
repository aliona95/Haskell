module Exercises_4
where

----------------1------------------
length1 :: [a] -> Int
length1 = sum . map (\x -> 1)

length2 :: [a] -> Int
length2 = foldr (\x -> (+ 1)) 0

----------------2------------------
any1 f xs
    | filter f xs == [] = False
    | otherwise = True

all1 f xs
    | length (filter f xs) == length xs = True
    | otherwise = False

any2 f xs = foldr (||) False (map f xs)

all2 f xs = foldr (&&) True (map f xs)

----------------3-------------------
unzip1 xs = foldr f ([], []) xs
  where
    f (x, y) (x1, y1) = (x:x1, y:y1)

----------------4-------------------
ff :: Integer -> [Integer] -> Integer
ff n = foldr f 0 . map (* 10) . filter (>= 0)
    where
     f elemSum x
         | elemSum + x <= n = elemSum + x
         | otherwise = x

--------------5-----------------
flip1 :: (a -> b -> c) -> (b -> a -> c)
flip1 f = \x y -> f y x

--------------6-----------------
total :: (Integer -> Integer) -> (Integer -> Integer)
total f n = (sum . map f) [0..n]

--------------7-----------------
iter1 :: Integer -> (a -> a) -> a -> a
iter1 n f
    | n > 0 = f . iter1 (n - 1) f
    | otherwise = id 

iter2 :: Integer -> (a -> a) -> a -> a
iter2 n f = foldr (.) id (replicate (fromIntegral n) f)

--------------8-----------------
splits xs = map f [0..length(xs)]
    where
     f n = ((take n xs), (drop n xs))
