numDivs :: Int -> Int -> Int
-- i must be 2
numDivs 1 _ = 1
numDivs n i = 
        if i * i < n
        then if n `mod` i == 0
            then 2 + numDivisores n (i+1)
            else numDivisores n (i+1)
        else if i * i == n
            then 3
            else 2

restDiv :: Float -> Float -> Float
restDiv x n
	| x >= n = restDiv (x - n) n
	| otherwise = x

toInt :: Float -> Float
toInt x = x - restDiv x 1

elementI :: [a] -> [a] -> Float -> a
elementI [] y _ = lastElement y
elementI (x:xs) y n
	  | sizeList y /= n + 1 = elementI xs (y ++ [x]) n 
	  | otherwise = lastElement y 

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
sumListSquare (x:xs) = x*x + sumList xs
