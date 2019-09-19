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

riemmansum :: Double -> Double -> Double -> Double -> (Double -> Double) -> Double
riemmansum a b n sum y =
    if (a < b - prec) then riemmansum (a + (b - a) / n) b n (sum + (y a) * ((b - a) / n)) y else sum
    where prec = 0.00001

integral :: Double -> Double -> (Double -> Double) -> Double
integral a b y = riemmansum a b n 0 y
    where n = 10000

dummyfunction :: Double -> Double
dummyfunction x = x

--input
firstArgument :: String -> String
firstArgument (' ':xs) = []
firstArgument (x:xs) = x:firstArgument xs

secondArgument :: String -> [Float]
secondArgument (' ':xs) = read xs :: [Float]
secondArgument (x:xs) = secondArgument xs

