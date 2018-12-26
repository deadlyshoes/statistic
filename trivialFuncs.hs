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