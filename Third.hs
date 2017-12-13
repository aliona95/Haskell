module Third
where
import Data.Char (toUpper)

--------------------1--------------------------
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
    | prefix xs ys = True
    | otherwise = substring xs (tail ys)

-- jei yra pasikartojimu pakeicia juos newSub
temp :: String -> String -> String -> String
temp [] _ st = st
temp _ _ [] = []
temp oldSub newSub st
    | prefix oldSub st = newSub ++ (temp oldSub newSub (drop (length oldSub) st))
    | otherwise = (head st) : (temp oldSub newSub (tail st))

subst :: String -> String -> String -> String
subst [] _ st = st
subst _ _ [] = []
subst oldSub newSub st
    | substring oldSub st = temp oldSub newSub st 
    | otherwise = temp oldSub newSub st

--------------------2--------------------------
whitespaces = ['\n', '\t', ' ']
punctuation = ['.', ',', ';', '-', ':']
spaces = whitespaces ++ punctuation

dropAllSpaces :: String -> String
dropAllSpaces [] = []
dropAllSpaces (x:xs)
    | elem x spaces = dropAllSpaces xs
    | otherwise = toUpper x : (dropAllSpaces xs)

isPalin :: String -> Bool
isPalin [] = True
isPalin xs
    | new_xs == reverse new_xs = True
    | otherwise = False
    where
     new_xs = dropAllSpaces xs

--------------------3--------------------------
dropSpaces :: String -> String
dropSpaces [] = []
dropSpaces (x:xs)
    | elem x spaces = dropSpaces xs
    | otherwise = (x:xs)

dropWord :: String -> String
dropWord [] = []
dropWord (x:xs)
    | elem x spaces = (x:xs)
    | otherwise = dropWord xs

wordCounter :: String -> Int
wordCounter [] = 0
wordCounter xs
    | dropSpaces xs == [] = 0
    | otherwise = wordCounter(dropWord(dropSpaces xs)) + 1

lineCounter :: String -> Int
lineCounter [] = 0
lineCounter (x:xs)
    | x == '\n' = 1 + lineCounter xs
    | otherwise = lineCounter xs

count:: String -> (Int,Int,Int)
count [] = (0, 0, 0)
count xs = (length xs, wordCounter xs, lineCounter xs)  
  
--------------------4--------------------------
dropNewLnSymbols :: String -> String
dropNewLnSymbols [] = []
dropNewLnSymbols xs
    | elem (head xs) "\n" = dropNewLnSymbols (tail xs)
    | otherwise = (head xs) : dropNewLnSymbols (tail xs)

dropEnd :: String -> String
dropEnd [] = []
dropEnd xs
    | elem (last xs) spaces = xs
    | otherwise = dropEnd(init xs)

getWord :: String -> String
getWord [] = []
getWord (x:xs)
    | elem x spaces = []
    | otherwise = x : getWord xs

justify :: String -> Int -> String
justify [] _ = []
justify xs line
    | length (getWord text) > line = error "ERROR"
    | (length text) > line && not(isLastSpace) && not(isLastNextSpace) = dropEnd getLine ++ "\n" ++ justify(drop (length(dropEnd getLine)) text) line
    | otherwise = (take line text) ++ "\n" ++ (justify (drop line text) line)
     where 
      text = dropNewLnSymbols xs
      getLine = take line text
      isLastSpace = (elem (last getLine) spaces)
      isLastNextSpace = (elem (last(take (line + 1) text)) spaces)

--------------------5--------------------------
data Shape = Circle Float (Float, Float)| Rectangle Float Float (Float, Float) deriving (Show, Ord, Eq)

overlaps :: Shape -> Shape -> Bool
overlaps (Rectangle width1 height1 (x1, y1)) (Rectangle width2 height2 (x2, y2)) = x1 < x2 + width2 && x1 + width1 > x2 && y1 < y2 + height2 && y1 + height1 > y2
overlaps (Circle r1 (x1, y1)) (Circle r2 (x2, y2)) = sqrt ((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)) < (r1 + r2)
overlaps (Circle r1 (x1, y1)) (Rectangle width height (x2, y2)) = overlapsCircleRect (Circle r1 (x1, y1)) (Rectangle width height (x2, y2))
overlaps (Rectangle width height (x2, y2)) (Circle r1 (x1, y1)) = overlapsCircleRect (Circle r1 (x1, y1)) (Rectangle width height (x2, y2))   

overlapsCircleRect :: Shape -> Shape -> Bool
overlapsCircleRect (Circle r (x1, y1)) (Rectangle width height (x2, y2))
    | distanceX > (width / 2 + r) || distanceY > (height / 2 + r) = False
    | distanceX <= (width / 2) || distanceY <= (height / 2) = True
    | otherwise = (distX * distX + distY * distY) <= (r * r)
    where
     distanceX = abs(x1 - x2 - width / 2) -- X atstumas tarp apskritimo ir staciakampio centru
     distanceY = abs(y1 - y2 - height / 2) 
     distX = distanceX - width / 2 -- atstumas nuo apskritimo centro iki staciakampio virsunes
     distY = distanceY - height / 2 

--------------------6--------------------------
-- loan (Person "Alice") (Book "KNYGA") ([BookStatus (Book "KNYGA") 45 Loaned, BookStatus (Book "KNYGA") 451 Free],[])
-- ([BookStatus (Book "KNYGA") 45 Loaned,BookStatus (Book "KNYGA") 451 Loaned],[Loan (Person "Alice") (Book "KNYGA") 451])

data Status = Loaned | Free | Locked deriving (Show, Eq)
data Book = Book Name deriving (Show, Eq)  
type Name = String
type ID = Int
data Person = Person Name deriving (Show, Eq)
data Loan = Loan Person Book ID deriving (Show, Eq) 
data BookStatus = BookStatus Book ID Status deriving (Show, Eq) 

loan :: Person -> Book -> ([BookStatus],[Loan]) -> ([BookStatus],[Loan])
loan person book (xs, ys)
    | fst isFree = ((updateBookStatus book xs), (ys ++ [Loan person book (snd isFree)])) 
    | otherwise = (xs, ys)
     where isFree = throughtBookStatuses book xs

throughtBookStatuses :: Book -> [BookStatus] -> (Bool,Int)
throughtBookStatuses _ [] = (False, 0)
throughtBookStatuses book xs
    | fst(isBookFree book (head xs)) = (True, snd(isBookFree book (head xs))) 
    | otherwise = throughtBookStatuses book (tail xs)

isBookFree :: Book -> BookStatus -> (Bool,Int)  -- Int => id
isBookFree (Book book_name) (BookStatus (Book name) id status)
    | book_name == name && status == Free = (True, id)
    | otherwise = (False, 0)

updateBookStatus :: Book -> [BookStatus] -> [BookStatus]
updateBookStatus _ [] = []
updateBookStatus book xs
    | fst(isBookFree book (head xs)) = [(BookStatus book (snd(isBookFree book (head xs))) Loaned)] ++ (tail xs)  
    | otherwise = [head xs] ++ (updateBookStatus book (tail xs))
