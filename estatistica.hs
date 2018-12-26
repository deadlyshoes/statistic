restoDiv :: Float -> Float -> Float
restoDiv x n
	| x >= n = restoDiv (x - n) n
	| otherwise = x

paraInt :: Float -> Float
paraInt x = x - restoDiv x 1

elementoI :: [a] -> [a] -> Float -> a
elementoI [] y _ = ultimoElemento y
elementoI (x:xs) y n
	  | tamanhoLista y /= n + 1 = elementoI xs (y ++ [x]) n 
	  | otherwise = ultimoElemento y 

ultimoElemento :: [a] -> a
ultimoElemento [x] = x
ultimoElemento (x:xs) = ultimoElemento xs 

somaLista :: [Float] -> Float
somaLista [] = 0
somaLista (x:xs) = x + somaLista xs

tamanhoLista :: [a] -> Float
tamanhoLista x = somaLista [1 | n <- x]

média :: [Float] -> Float
média x = somaLista x / tamanhoLista x

mediana :: [Float] -> Float
mediana x
	| restoDiv (tamanhoLista x) 2 == 0 = (elementoI x [] ((tamanhoLista x) / 2 - 1) + elementoI x [] ((tamanhoLista x) / 2)) / 2
	| otherwise = elementoI x [] (paraInt (tamanhoLista x / 2))
