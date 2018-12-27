import trivialFuncs

-- Always assume that all lists are in ascending order

mean :: [Float] -> Float
mean x = sumList x / sizeList x

median :: [Float] -> Float
median x
	| restDiv (sizeList x) 2 == 0 = (elementI x [] ((sizeList x) / 2 - 1) + elementI x [] ((sizeList x) / 2)) / 2
	| otherwise = elementI x [] (toInt (sizeList x / 2))
	
-- mode :: -> 
-- mode  = 

range :: [Float] -> Float
range (x:xs) = lastElement xs - x

sampleVariance :: [Float] -> Float
sampleVariance x = (sumListSquare x - 2*sumList x*mean x + mean x * mean x) / (sizeList (x) - 1)
