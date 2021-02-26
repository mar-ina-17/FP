drop' :: Integer -> [Integer] ->[Integer]
drop' _ [] = []
drop' 0 xs = xs
drop' n (_:xs) = drop' (n-1) xs

checkVowels :: Char -> Bool
checkVowels c = elem c "aeiouy"

countVowels :: String -> Int
countVowels word = length $ filter checkVowels word


isPrefix :: String -> String ->Bool
isPrefix s1 s2 
    | take 1 s1 == take 1 s2 = True 
    | take 2 s1 == take 2 s2 = True 
    |otherwise = False
    
strContains :: String-> String -> Bool
strContains _ [] = True
strContains [] _ = False
strContains xs ys
    | isPrefix xs ys = True
    | strContains (tail xs) ys= True
    | otherwise = False

isPrime :: Int -> Bool
isPrime n = length [x | x <- [2..(div n 2)], mod n x == 0]==0

primeDecompose :: Int -> (Int, Int)
primeDecompose n =  if null ts then (0, 0) else head ts where ts = [(x, n - x) | x <- [3..(div n 2)], isPrime x && isPrime (n-x)]

rotateN :: String -> Int -> String 
rotateN word n = drop n word  ++ take n word

rotations :: String -> [String]
rotations s = [rotateN s x | x <- [1..length s]] 

myMerge :: (Ord a) => [a] -> [a] -> [a]
myMerge [] ys = ys
myMerge xs [] = xs
myMerge (x:xs) (y:ys)
  | x <= y = x:myMerge xs (y:ys)
  | otherwise = y:myMerge (x:xs) ys
myMergeSort :: (Ord a) => [a] -> [a]
myMergeSort xs
  | len < 2 = xs
  | otherwise = myMerge (myMergeSort l) (myMergeSort r) where
      len = length xs
      (l, r) = splitAt (div len 2) xs

bwt :: String -> String
bwt xs = [last xs | xs <- myMergeSort $ rotations xs]

lolSum :: [Int] -> Int
lolSum xs = foldl (\agg x -> if even x then agg - x else agg + x) 0 xs

isInAlphabet :: String -> (String -> Bool)
isInAlphabet alph = foldl (\agg x -> agg && elem x alph) True

isInAlphabet2 :: String -> (String -> Bool)
isInAlphabet2 alphabet = and . map (\c -> elem c alphabet)