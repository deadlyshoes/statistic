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

--integral adapted from https://rextester.com/KHC32406
riemanBlock :: (Double, Double) -> (Double -> Double) -> Double
riemanBlock (a, b) f
  | a > b     = riemanBlock (b, a) f
  | otherwise       = ((f a) + (f b)) / 2.0 * (b - a)

riemanIterator :: [Double] -> (Double -> Double) -> [Double]
riemanIterator [a] _ = []
riemanIterator (p:q:ps) f = (riemanBlock (p, q) f) : (riemanIterator (q:ps) f)

riemanSum :: [Double] -> (Double -> Double) -> Double
riemanSum partition f = sum (riemanIterator partition f)

createPartition :: (Double, Double) -> Double -> [Double]
createPartition (a, b) n
  | a > b = createPartition (b, a) n
  | otherwise = [a, b + (b - a)/n .. b]

integralIterator :: Double -> Double -> (Double, Double) -> (Double -> Double) -> Double -> Double
integralIterator lastIntegral lastNum (a, b) f prec
  | prec > abs (lastIntegral - currentIntegral) = currentIntegral
  | otherwise = integralIterator currentIntegral currentNum (a, b) f prec
  where currentNum = lastNum * 2
        currentIntegral = riemanSum (createPartition (a, b) currentNum) f

integral :: (Double, Double) -> (Double -> Double) -> Double
integral (a, b) f = integralIterator (riemanSum [a, b] f) 1 (a, b) f prec
  where prec = 0.000000001

--input
firstArgument :: String -> String
firstArgument (' ':xs) = []
firstArgument (x:xs) = x:firstArgument xs

secondArgument :: String -> [Float]
secondArgument (' ':xs) = read xs :: [Float]
secondArgument (x:xs) = secondArgument xs

