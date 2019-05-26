import TrivialFuncs

-- Always assume that all lists are in ascending order

mean :: [Float] -> Float
mean x = sumList x / sizeList x

median :: [Float] -> Float
median x
    | restDiv (sizeList x) 2 == 0 = (elementI x ((sizeList x) / 2 - 1) + elementI x ((sizeList x) / 2)) / 2
    | otherwise = elementI x (toInt (sizeList x / 2))

mode :: [Float] -> [Float] 
mode x 
    | x == result = []
    | otherwise = result
    where result = map (\(y, _) -> y) (filter (\(_, z) -> z == maximum (repeatAll x)) (squash' x (repeatAll x))) 
    
range :: [Float] -> Float
range (x:xs) = lastElement xs - x

sampleVariance :: [Float] -> Float
sampleVariance x = (sumListSquare x - 2*sumList x*mean x + mean x * mean x) / (sizeList (x) - 1)
