module Second
where
import Data.Char
import Data.Char (toUpper)

----------------1--------------------
average :: [Float] -> Float
average (xs) = list_sum(xs) / (fromIntegral(length xs))
    where
     list_sum [] = 0
     list_sum (x:xs) = x + list_sum xs

----------------2--------------------
divides :: Integer -> [Integer]
divides 0 = []
divides n = n : (divNum n (div n 2))

divNum n m 
    | m == 0 = []
    | mod n m == 0 = m : (divNum n (m - 1))
    | otherwise = (divNum n (m - 1))

divides_1 :: Integer -> [Integer]
divides_1 n = [m| m <- [1..n], mod n m == 0]

isPrime :: Integer -> Bool
isPrime n
    | n < 0 = error "negative n"
    | otherwise = length(divides_1 n) == 2

----------------3--------------------
prefix :: String -> String -> Bool
prefix [] [] = True
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = False

substring :: String -> String -> Bool
substring [] _ = True
substring _ [] = False
substring (xs) (ys) 
    | prefix xs ys == False = substring xs (tail ys)
    | otherwise = True

----------------4--------------------
permut :: [Integer] -> [Integer] -> Bool
permut [] [] = True
permut [] _  = False
permut _ []  = False
permut (x:xs) (y:ys)
    | length xs /= length ys = False
    | otherwise = permut (remove (x:xs) x) (remove (y:ys) x) 
    where
     remove [] _ = []
     remove (y:ys) x
         | x == y = ys
         | otherwise = y:(remove ys x)

---------------5---------------------
capitalise :: String -> String
capitalise xs = [toUpper x | x <- xs, ((x >= 'a' && x <= 'z') || (x >= 'A' && x <= 'Z'))]

---------------6---------------------
totalItem :: [(String,Float)] -> [(String,Float)]
totalItem [] = []
totalItem (x:xs)
    | xs == [] = [x] 
    | otherwise = [(head (temp (x:xs)))] ++ totalItem ((tail (temp (x:xs))))

temp :: [(String,Float)] -> [(String,Float)]
temp [] = []
temp (x:xs)
    | xs == [] = [x]
    | fst x == fst y = temp ([y] ++ (tail xs)) 
    | fst x /= fst y = (temp ([x] ++ (tail xs))) ++ [y] 
    where
     y = check x (head xs)

check :: (String,Float) -> (String,Float) -> (String,Float)
check x y
    | fst x == fst y = (fst x, snd x + snd y) 
    | fst x /= fst y = y

--------------------------------------
itemDiscount :: String -> Integer -> [(String,Float)] -> [(String,Float)]
itemDiscount _ _ [] = []
itemDiscount name discount (x:xs)
    | not(discount >= 0 && discount <= 100) = error "discount ranging from 0% until 100%"
    | fst x == name = (name, (snd x) * f) : (itemDiscount name discount xs)
    | otherwise = x : (itemDiscount name discount xs)
    where
     f = ((100 - fromIntegral(discount)) / 100)
