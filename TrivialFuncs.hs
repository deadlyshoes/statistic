module TrivialFuncs where

numDivs :: Int -> Int -> Int
-- i must be 2
numDivs 1 _ = 1
numDivs n i = 
    if i * i < n
    then if n `mod` i == 0
        then 2 + numDivs n (i+1)
        else numDivs n (i+1)
    else if i * i == n
        then 3
        else 2

toInt :: Float -> Float
toInt x = x - restDiv x 1

restDiv :: Float -> Float -> Float
restDiv x n
    | x >= n = restDiv (x - n) n
    | otherwise = x

squash' :: [a] -> [b] -> [(a, b)]
squash' [] _ = []
squash' _ [] = []
squash' (x:xs) (y:ys) = (x, y):squash' xs ys

elementI :: [a] -> Float -> a 
elementI x n = (\[(z, _)] -> z) . filter (\(_, y) -> y == round n) $ squash' x [0..length x - 1]

lastElement :: [a] -> a
lastElement [x] = x
lastElement (x:xs) = lastElement xs

sumList :: [Float] -> Float
sumList [] = 0
sumList (x:xs) = x + sumList xs

sizeList :: [a] -> Float
sizeList x = sumList [1 | n <- x]

sumListSquare :: [Float] -> Float
sumListSquare [] = 0
sumListSquare (x:xs) = x*x + sumListSquare xs

repeatTimes :: Float -> [Float] -> Int
repeatTimes x [] = 1
repeatTimes x (y:ys)
    | y == x = 1 + repeatTimes x ys
    | otherwise = repeatTimes x ys

repeatAll :: [Float] -> [Int]
repeatAll [] = []
repeatAll (x:xs) = repeatTimes x xs:repeatAll xs

riemmansum :: Double -> Double -> Int -> (Double -> Double) -> Double
riemmansum a b n y sum
    | a < b = riemmansum (a + (b - a) / n) b n (sum + (y a)) y
	| otherwise = sum

integral :: Double -> Double -> (Double -> Double) -> Double
integral a b y = riemmansum a b n 0.0 y
    where n = 1000

y :: Double -> Double
y x = x

--input
firstArgument :: String -> String
firstArgument (' ':xs) = []
firstArgument (x:xs) = x:firstArgument xs

secondArgument :: String -> [Float]
secondArgument (' ':xs) = read xs :: [Float]
secondArgument (x:xs) = secondArgument xs

