sumTwoNumbers :: Integer -> Integer -> Integer
sumTwoNumbers m n = m + n

allGreaterThan :: Integer -> [Integer]

allGreaterThan n = n:allGreaterThan (n + 1)

countAllDigits :: Integer -> Integer
countAllDigits n = if(n == 0) then 0 else 1 + countAllDigits (div n 10)

countAllDigitsSecond ::  Integer -> Integer
countAllDigitsSecond n
    | n < 10 = 1
    | otherwise = 1 + countAllDigitsSecond (div n 10)


factorialG :: Integer -> Integer
factorialG n 
   | n == 0 = 1
    | otherwise = n* factorialG(n-1)

factorial :: Integer -> Integer
factorial  1= 1
factorial n = n*factorial(n-1)

isPrefix :: String -> String ->Bool
isPrefix s1 s2 
    | take 1 s1 == take 1 s2 = True 
    | take 2 s1 == take 2 s2 = True 
    |otherwise = False


count :: Char -> String -> Integer
count x [] = 0
count x (c:cs) | x == c = 1 + count x cs
               | otherwise = count x cs

listToTuple:: [Int] -> [(Int, Int)]
listToTuple [] =[]
listToTuple [x] = []
listToTuple (x:y:xs) = (x,y):listToTuple xs

pairProcessor:: (Int -> Int-> Int) -> ([Int -> [Int]])
pairProcessor f = (\xs -> map(\)) (listToTuple xs)

isBST:: (Ord a) ->Btree a -> Bool
isBST Nil = True
isBST (Node root left right) = greaterThanAllOnTheLeft root left && smallerThanAllOnTheRight root right && isBST left &&isBST right