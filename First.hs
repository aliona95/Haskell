module First
where

import Test.QuickCheck

--------------1----------------
nAnd_1 :: Bool -> Bool -> Bool
nAnd_1 x y = not (x && y)

nAnd_2 :: Bool -> Bool -> Bool
nAnd_2 True x  = not x
nAnd_2 False _ = True

nAnd_3 :: Bool -> Bool -> Bool
nAnd_3 True True   = False
nAnd_3 True False  = True
nAnd_3 False True  = True
nAnd_3 False False = True

-------------------2-----------------------------
prop_nAnd :: Bool -> Bool -> Bool
prop_nAnd x y = (nAnd_1 x y == nAnd_2 x y) && (nAnd_2 x y == nAnd_3 x y)

-------------------3-----------------------------
nDigits :: Integer -> Int
nDigits n = length (show (abs n))

-------------------------------------------------

-------------------4-----------------------------
nRoots :: Float -> Float -> Float -> Int 
nRoots a b c
    | a == 0.0 = error "the first argument should be non-zero!"
    | b ^ 2 > 4.0 * a * c = 2       -- 1 2 -3
    | b ^ 2 == 4.0 * a * c = 1      -- 1 2 1
    | b ^ 2 < 4.0 * a * c = 0       -- 2 1 1

-------------------5-----------------------------
smallerRoot :: Float -> Float -> Float -> Float
smallerRoot a b c
    | nRoots a b c == 0 = error "no roots"
    | otherwise = (-b - sqrt(b ^ 2 - 4 * a *c)) / (2 * a)

largerRoot :: Float -> Float -> Float -> Float
largerRoot a b c
    | nRoots a b c == 0 = error "no roots"
    | otherwise = (-b + sqrt(b ^ 2 - 4 * a *c)) / (2 * a)

-------------------6-----------------------------
power2 :: Integer -> Integer
power2 n
    | n == 0 = 1
    | n > 0 = power2(n - 1) * 2
    | otherwise = 0

-------------------7-----------------------------
mult :: Integer -> Integer -> Integer
mult m n
    | n > 0 && m > 0 = mult m (n - 1) + m
    | n < 0 && m < 0 = mult (-m) (-n)
    | n == 0 || m == 0 = 0
    | otherwise = -mult (abs m) (abs n)

prop1 :: Integer -> Integer -> Bool
prop1 m n = mult m n == m * n
-------------------8-----------------------------
prod :: Integer -> Integer -> Integer 
prod m n
    | m < n = m * prod m (n - 1)
    | m == n = m 
    | otherwise = error "m > n"

fact :: Integer -> Integer
fact n = prod 1 n









