--------------------------------------
--									--
--		PRÁCTICA HASKELL			--
--	 Ignacio Iker Prado Rujas		--
--									--
--------------------------------------

import Data.Function (on)
import Data.Char
import Data.List

--Menu principal
decode:: IO ()
decode = do
	putStrLn "\nDECODIFICADOR:"
	putStrLn "(1) Decodificacion semiautomatica (usando frecuencia de aparicion de letras en el castellano)."
	putStrLn "(2) Codificacion manual."
	putStrLn "(3) Decodificacion manual."
	putStrLn "(4) Frecuencia de caracteres en un texto."
	putStrLn "(5) Frecuencia de palabras en un texto."
	putStrLn "\nOpcion en rango [1..5] o Intro para terminar: "	
	opcion <- getLine
	case opcion of
		"1" -> semiAut
		"2" -> codManual
		"3" -> decManual
		"4" -> frecCaracteres
		"5" -> frecPalabras
		_ 	-> putStrLn "Bye, bye..."

keyFrecs = [('e',230965),('a',201484),('o',163816),('s',126466),('n',109456),
	('r',102093),('i',91172),('l',89617),('d',87762),('u',80062),('t',63233),('c',60028),
	('m',45016),('p',35905),('q',32497),('y',25433),('b',24412),('h',20422),('v',17969),
	('g',17623),('j',10619),('f',7960),('z',6493),('x',406),('w',263),('k',128)]
		
--Decodificacion parcial basada en las frecuencias de los caracteres del Quijote
--Se requiere ayuda del usuario que guía el resto de la decodificacion
semiAut:: IO ()
semiAut = do
	putStrLn "Introduce el nombre del fichero a descifrar (sin la extension): "
	coded <- abreFich
	let codedFrecs1 = sortBy (flip (compare `on` snd)) $ frecAbs $ map (toLower) coded
	let codedFrecs = filter (\(x, _) -> x /= '.' && x /= ',' && x /= ':' 
		&& x /= ' ' && x /= '\n') codedFrecs1
	--Para ahorrar tiempo de computo, introducimos manualmente las frecuencias 
	--de letras del Quijote. Para calcularlo sobre el fichero Quijote.txt, 
	--comenta el "keyFrecs = ..." anterior y descomenta las 5 líneas siguientes:
	--putStrLn "\nIntroduce el nombre del fichero referencia: "
	--key1 <- abreFich
	--let key = map (toLower) key1 
	----drop 29 es para eliminar caracteres como ',', ':'... y take 26 se queda con los 
	----caracteres del alfabeto. Imprime key en la consola para verlo mas claro
	--let keyFrecs1 = take 26 $ drop 29 $ sort $ frecAbs key 
	--let keyFrecs = sortBy (flip (compare `on` snd)) keyFrecs1
	let decodificacion1 = sort $ zip (fst $ unzip codedFrecs) (fst $ unzip keyFrecs) --Ojo: codedFrecs puede ser mas pequeño que keyFrecs, pero zip lo arregla
	let decodificacion = (decodificacion1 ++ [(toUpper a, toUpper b) | (a,b) <- decodificacion1]) 
	putStrLn ("\nDECODIFICACION INICIAL:\n"++show decodificacion)
	let newText = decodifica coded decodificacion
	putStrLn("\nTEXTO CODIFICADO:\n")
	putStrLn(coded)
	putStrLn("\nTEXTO (semi)DECODIFICADO:\n")
	putStrLn(newText)
	decodFinal <- cambioManual newText decodificacion
	decode
	
--Lee nombre de un fichero, lo procesa y devuelve el texto que contiene
abreFich :: IO String
abreFich = do
	file <- getLine
	texto <- readFile (file++".txt")
	return texto
	
--Función que dada una lista de elementos y un elemento, elimina 
--las apariciones de ese elemento en la lista
borraX :: Eq a => a -> [a] -> [a]
borraX _ [] = []
borraX x (y:xs) = 	if x == y then borraX x xs
					else 		   y:(borraX x xs)
	
--Función que dada una lista de elementos, elimina los duplicados
borraDupl :: Eq a => [a] -> [a]
borraDupl [] = []
borraDupl (x:xs) = x:borraDupl [y | y <- xs, y /= x]

--Función que dada una lista de elementos, devuelve una lista
--sin repeticiones de pares: (elemento, frecuencia absoluta)
frecAbs :: Eq a => [a] -> [(a, Int)]
frecAbs [] = []
frecAbs (x:xs) = [(x, 1 + length [y | y <- xs, y == x])] ++ frecAbs (borraX x xs)

--Función que dada una codificación de letras (antigua, nueva)
--hace el cambio en todo un texto y lo devuelve
--swap :: (Char, Char) -> String -> String
--swap _ [] = []
--swap (a,b) (x:xs) = if x == a then b:(swap (a,b) xs)
--					else		   x:(swap (a,b) xs)

--Función que dada una letra x busca el la lista de pares ys
--si (x, b) \in ys, y devuelve la letra nueva: b
buscaElCambio :: Char -> [(Char,Char)] -> Char
buscaElCambio x ys
	| null listOfPairs = x
	| otherwise 	   = snd $ head listOfPairs
	where listOfPairs  = [(x, b) | (a, b) <- ys, x == a]
					
--Función que dada un texto y una codificación de la forma
--[('a', 'j'), ('h', 'z'), ...] decodifica el texto segun 
--esa clave, siendo fst(a,b) la antigua y snd(a,b) la nueva
decodifica :: String -> [(Char,Char)] -> String
decodifica [] ys = []
decodifica (x:xs) ys = (buscaElCambio x ys):(decodifica xs ys)

--Funcion auxiliar que llama a su vez a la funcion "decodifica" para cambiar
--el caracter antiguo por el nuevo y viceversa (para mantener la biyeccion)
cambioManualAux :: ([(Char, Char)],(Char, Char)) -> String -> IO [(Char, Char)]
cambioManualAux dupla texto = do
	let decodificacion = fst dupla
	let antiguo 	   = fst $ snd dupla
	let nuevo 		   = snd $ snd dupla
	let newText = decodifica texto [(toLower antiguo, toLower nuevo), (toLower nuevo, toLower antiguo), (toUpper antiguo, toUpper nuevo), (toUpper nuevo, toUpper antiguo)]
	let decodificacionNueva1 = actualizaClave decodificacion (antiguo, nuevo)
	putStrLn("\nTEXTO TRAS EL CAMBIO:\n")
	putStrLn(newText)
	if (length decodificacionNueva1) < 40 then do
		let decodificacionNueva = take 30 decodificacionNueva1
		putStrLn("\nDECODIFICACION TRAS EL CAMBIO:\n"++show decodificacionNueva)
		decodificacionNueva' <- cambioManual newText decodificacionNueva
		return decodificacionNueva'
	else do
		let decodificacionNueva = decodificacionNueva1
		putStrLn("\nDECODIFICACION TRAS EL CAMBIO:\n"++show decodificacionNueva)
		decodificacionNueva' <- cambioManual newText decodificacionNueva
		return decodificacionNueva'

--Pide al usuario dos caracteres a intencambiar. Intro para volver al menu
cambioManual :: String -> [(Char, Char)] -> IO [(Char, Char)]
cambioManual texto decodificacion = do
	putStrLn "\nIntroduce el caracter antiguo y el nuevo separados por un espacio (o Intro para volver al menu): "
	par <- getLine
	if length par == 3 then do
		decodificacion' <- cambioManualAux (decodificacion, (par !! 0, par !! 2)) texto
		return decodificacion'
	else do 
		guardaTexto texto
		return decodificacion

--Función que toma una codificación y un par de caracteres a intercambiar, 
--actualizando dicha codificación con ayuda de la función cruza
actualizaClave :: [(Char, Char)] -> (Char,Char) -> [(Char, Char)]
actualizaClave ys (antiguo, nuevo) = [(a,b) | (a,b) <- ys, b /= toUpper nuevo, b /= toUpper antiguo, b /= toLower nuevo, b /= toLower antiguo]++ 
											 (cruza unoL otroL) ++ (cruza unoU otroU)
	where {unoU  = head [(toUpper a, toUpper b) | (a,b) <- ys, b == antiguo]; otroU = head [(toUpper a, toUpper b) | (a,b) <- ys, b == nuevo];
		   unoL  = head [(toLower a, toLower b) | (a,b) <- ys, b == antiguo]; otroL = head [(toLower a, toLower b) | (a,b) <- ys, b == nuevo]}

--Función que toma (a,b) y (c,d), devolviendo (a,d) y (c,b)
--Necesaria para actualizar la codificacion cuando esta cambia
cruza :: (Char, Char) -> (Char, Char) -> [(Char, Char)]
cruza uno otro = ([(fst uno, snd otro)] ++ [(fst otro, snd uno)])
		
--Lee el nombre de un fichero y crea un fichero con
--ese título, guardando el texto dado en él
guardaTexto :: String -> IO ()
guardaTexto texto = do
	putStrLn "\nIntroduce el nombre del fichero (sin la extension) donde quieres guardar el resultado (o 'no' para no hacerlo): "	
	destino <- getLine
	case destino of
		"no" -> putStrLn("\nOk, no se ha guardado el resultado.")
		_    -> writeFile (destino++".txt") texto
		
--Codifica un texto de un fichero con la clave que quiera el usuario
--OJO: Aqui va todo a minusculas para poder codificar con los signos de 
--puntuacion, ya que si permitimos mayusculas se plantea el problema: 
--Si cambio 'a' por ' ', ¿qué le corresponde a 'A'? Se fastidia la biyección
codManual :: IO ()
codManual = do
	putStrLn "Introduce el nombre del fichero a cifrar (sin la extension): "
	origen1 <- abreFich
	let origen = map (toLower) origen1
	putStrLn("\nTEXTO ORIGINAL A CIFRAR:\n")
	putStrLn(origen)
	putStrLn("\n\n")
	putStrLn("\nSe permite los cambios de ' ', ',', '.', ':' ademas de las letras ['a'..'z'].\n")
	decodificacion <- cambioManual origen ([(' ', ' '), (',', ','), ('.', '.'), (':', ':')] ++ [(a,a) | a <- ['a'..'z']])
	putStrLn "\nIntroduce el nombre del fichero (sin la extension) donde quieres guardar la clave (o 'no' para no hacerlo): "	
	destino <- getLine
	case destino of
		"no" -> putStrLn("\nOk, no se ha guardado la clave (espero que te la sepas de memoria).")
		_    -> writeFile (destino++".txt") (concat [("("++[a]++", "++[b]++")\n" ) | (a,b) <- (sort decodificacion)])
	decode
	
--Decodifica un texto de un fichero con los cambios que quiera el usuario
--OJO: De nuevo, no permitimos mayusculas para permitir signos de puntuación
decManual :: IO ()
decManual = do
	putStrLn "Introduce el nombre del fichero a descifrar (sin la extension): "
	origen1 <- abreFich
	let origen = map (toLower) origen1
	putStrLn("\nTEXTO ORIGINAL A DESCIFRAR:\n")
	putStrLn(origen)
	putStrLn("\n\n")
	putStrLn("\nSe permite los cambios de ' ', ',', '.', ':' ademas de las letras ['a'..'z'].\n")
	decodificacion <- cambioManual origen ([(' ', ' '), (',', ','), ('.', '.'), (':', ':')] ++ [(a,a) | a <- ['a'..'z']])
	putStrLn "\nIntroduce el nombre del fichero (sin la extension) donde quieres guardar la clave (o 'no' para no hacerlo): "	
	destino <- getLine
	case destino of
		"no" -> putStrLn("\nOk, no se ha guardado la clave (espero que te la sepas de memoria).")
		_    -> writeFile (destino++".txt") (concat [("("++[a]++", "++[b]++")\n" ) | (a,b) <- (sort decodificacion)])
	decode
	
--Calcula la frecuencia de cada caracter de un texto dado	
--OJO: Contamos indistintamente mayusculas y minusculas
frecCaracteres :: IO ()
frecCaracteres = do
	putStrLn "Introduce el nombre del fichero del que extraer las frecuencias de caracteres (sin la extension): "
	texto1 <- abreFich
	let texto = map (toLower) texto1 
	let textoFrecs = sortBy (flip (compare `on` snd)) $ frecAbs texto
	putStrLn("\n(CARACTER, \t FREC. ABSOLUTA, \t PORCENTAJE DE APARICION)\n")
	let total = fromIntegral $ sum $ snd $ unzip textoFrecs in putStrLn(concat [("("++(show a)++", \t"++(show b)++",\t\t"++(show (((fromIntegral b)/total)*100))++"%)\n" ) | (a,b) <- textoFrecs])
	decode
	
--Calcula la frecuencia de cada palabra de un texto dado	
--OJO: Contamos indistintamente mayusculas y minusculas
frecPalabras :: IO ()
frecPalabras = do	
	putStrLn "Introduce el nombre del fichero del que extraer las frecuencias de palabras (sin la extension): "
	texto1 <- abreFich
	let texto = words $ map toLower $ quitaSignPunt $ texto1 
	let textoFrecs = sortBy (flip (compare `on` snd)) $ frecAbs texto
	putStrLn("\n(PALABRA, \t FREC. ABSOLUTA, \t PORCENTAJE DE APARICION)\n")
	let total = fromIntegral $ sum $ snd $ unzip textoFrecs in putStrLn(concat [("("++(show a)++", \t\t"++(show b)++",\t\t"++(show (((fromIntegral b)/total)*100))++"%)\n" ) | (a,b) <- textoFrecs])
	decode
	
--Elimina signos de puntuacion de un String
quitaSignPunt :: String -> String
quitaSignPunt []       = []
quitaSignPunt ('.':xs) = quitaSignPunt xs
quitaSignPunt (',':xs) = quitaSignPunt xs
quitaSignPunt (':':xs) = quitaSignPunt xs
quitaSignPunt (x:xs)   = x:(quitaSignPunt xs)