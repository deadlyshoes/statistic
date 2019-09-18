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
sampleVariance x = (sumListSquare x - 2 * mean x * sumList x + sizeList x * mean x * mean x) / (sizeList x - 1)

standardDeviation :: [Float] -> Float
standardDeviation = sqrt . sampleVariance

main = do
    inp <- getLine
    if (firstArgument inp) == "mean" then print $ mean (secondArgument inp)
    else if (firstArgument inp) == "median" then print $ median (secondArgument inp)
    else if (firstArgument inp) == "mode" then print $ mode (secondArgument inp)
    else if (firstArgument inp) == "range" then print $ range (secondArgument inp)
    else if (firstArgument inp) == "sampleVariance" then print $ sampleVariance (secondArgument inp)
    else if (firstArgument inp) == "standardDeviation" then print $ standardDeviation (secondArgument inp) else main
