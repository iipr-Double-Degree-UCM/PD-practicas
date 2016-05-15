--Mi primera funcion en Haskell: Calcular el doble
doble x = x*2

--Primer factorial
{-factorial n = if n>1 then n * factorial (n-1)
			  else 1 
-}

--Segundo factorial
factorial 1 = 1
factorial n = n*factorial(n-1)

--Funcion sandwich, ha costado -.- 
sandwich xs ys = let us = xs++xs; vs = ys++ys 
				 in  us ++ vs ++ us

--Primera versión de una f
{-f 0 y = True
f x 0 = False
f x y = f (x-1) (y-1)-}

--Version un poco mas guay de la f
f x y 
		| x < 0 || y < 0 = error ("los valores "++show x++" y "++show y++" deben ser positivos")
		| x == 0 = True
		| y == 0 = False
		| otherwise =  f (x-1) (y-1)
		
--Soluciones a una ecuacion de 2º grado con coefs reales
--Dos soluciones reales, solucion doble o parte real e imaginaria
soluciones a b c
			| d > 0  = [0,e+s,e-s]
			| d == 0 = [1,e]
			| d < 0  = [2,e,sqrt (-d)/2*a]
			where  d = b^2-4*a*c;
				   e = -b/2*a;
				   s = sqrt d / 2*a

--Longitud de una lista
long :: [a] -> Int
long [] = 0
long (a:b) = 1 + long b

--Cosa que es mas facil con map, ver la siguiente funcion
longLong :: [[a]] -> [Int]
longLong [] = []
longLong (a:b) = long a : (longLong b)

--Uso de map
inc :: Num a => a -> a
inc x = x + 1
incList :: Num a => [a] -> [a]
incList[] = []
incList x = map inc x

--Lambda expresiones
cuenta x = foldl (\a y -> if (x == y) then a+1 else a) 0 

--Primos en [1..1000] 1000 primeros
primo x 
		| (x == 1) = False
		| (x == 2) = True
		| ([y |y <- [2..(ceiling ((fromInteger x)/2))], mod x y == 0] == []) = True
		| otherwise = False
prim1000 = [x | x <- [1..1000], primo x]

--Funcion de la diapositiva 118
jota n = [(x,y) | x <- [1..n] , y <- [1..x]]

--Ternas pitagoricas con hipotenusas hasta el valor n
pita n = [[x,y,z] | z <- [1..n], x <- [1..z-1], y <- [1..x-1], z^2 == x^2+y^2]

--Conjunto de cuadrados perfectos entre a y b
cuadraditos a b = [x | x <- [a..b],
	(sqrt (fromInteger x) == fromInteger (floor (sqrt (fromInteger x))))]

--EN PROCESO:
--Conjunto potencia con listas intensionales
--pot :: [a] -> [[a]]
--pot [] = [[]]
--pot(x:xs) = [] : [x:ys | ys <- pot xs]

g n = [[x..n] | x <- [0..n]]

--sufs [] = [[]]
--sufs (xs:x) = [ys:x | ys <- sufs xs] : []

--Ejercicio 1a Examen 
examen1a n = concat [[0], concat [[x,0-x] | x <- [1..n]]]

--Ejercicio 1b Examen
--examen1b [] x y = y
--examen1b (a:as) x y = x (f as  x y) a

--Ejercicio 3a Examen
cuenta1 :: Eq a => a -> [a] -> Int
cuenta1 x [] = 0
cuenta1 x (x':xs) = if x==x' then (1 + cuenta x xs) else cuenta x xs
apariciones :: Eq a => [a] -> [Int]
apariciones [] = []
apariciones (x:xs) = [cuenta i (x:xs) | i <- (x:xs)]

--Ejercicio 3b Examen
divisores :: Int -> [Int]
divisores n = [i | i <- [1..n], mod n i == 0]
divHasta :: Int -> [(Int,Int)]
divHasta n = [(i, j) | i <- [1..n], j <- [length (divisores i)]]