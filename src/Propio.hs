module Propio
  where 

{-Desarrollo propio para encontrar solucion al problema del concurso-}

-- Funcion para combinar un elemento con todos los elementos de una lista formando parejas
combinar :: a -> [a] -> [[a]]
combinar e [x] = [[e,x]]                               --  Caso base
combinar e (x:xs) = [e,x] : combinar e xs              -- Natural recursion 

parejas :: [a] -> [[a]]
parejas (x:y:[]) = [[x,y]]                             -- Caso Base
parejas (x:xs) = (combinar x xs) ++ parejas xs         -- Natural recursion

pares :: [a] -> [[a]]
pares (x:[])   = [[x]]
pares (x:y:[]) = [[x,y]]                               -- Caso base cuando quedan solo dos.
pares (x:xs)   = [[x,y] | y <- xs] ++ pares xs         -- Natural recursion 

{-Para realizar las operaciones-}
type Operador = String 
operadores :: [Operador]
operadores  = ["suma", "resta", "mult", "div"] 

{-[1,4,6,7] -> Parejas [[1,4],[1,6],[1,7]...]  -> tuplas operacion -} 

data Operacion = Operacion  String Int Int  deriving (Show)

operar :: Operacion -> Int                                                  
operar (Operacion op x y) 
   | op == "suma"  = x + y
   | op == "resta" = x - y
   | op == "mult"  = x * y
   | op == "div"   = x `div` y

{-Necesitamos una funcion que nos diga si las operaciones son validas-}
isValid :: Operacion -> Bool
isValid (Operacion op x y)
   | op == "suma"  = True           -- Siempre se pueden sumar
   | op == "resta" = x > y          -- Solo cuando nos da un positivo
   | op == "mult"  = cMult x y      -- Solo cuando se pueden multiplicar
   | op == "div"   = cDiv  x y      -- Solo si son divisibles 
     where
       cMult x y
         | (x == 1) || (y == 1) = False    -- No nos interesa multiplicar por 1
         | (x == 0) || (y == 0) = False    -- No nos interesa multiplicar por 0
         | otherwise            = True     -- En otro caso multiplicamos.
       cDiv x y
         |  x `rem` y /= 0      = False    -- Solo queremos los divisibles
         | (y == 1)             = False    -- No nos interesa dividir por 1
         | (y == 0) = False                -- No nos interesa dividir por 0  
         | otherwise            = True     -- Si no se cumple lo anterior podemos dividir

{-Ahora necesitamos una funcion que de una lista de pares nos de todas las combinacines posibles con los 
operadoes -} 
getOperaciones pares oprs = [sal |
                             op    <- oprs,
                             [x,y] <- pares, 
                             let sal = Operacion op  x y,
                             isValid sal]

{-Recibe un estado inicial y una par y devuelve el estado sin ese par y un valor añadido de la 
actualizar la operacion
[1,2,3,4] Operacion "suma" 2 3 -> [1,5,4]-}  
type Estado = [Int]
type Paso   = (Operacion, Estado)
-- Ejemplo de uso getEstados [1,5,7,8] (Operacion "suma" 1 5)  = [6,7,8]
getEstados :: Estado -> Operacion -> Estado
getEstados xs  o@(Operacion op  x y) = (operar o) : filter (noestan) xs  
   where 
    noestan z = (z /= x && z /= y)

{-Ahora obtenemos de cada estado representado por una lista de numeros una lista de pares y con ella 
una lista de operaciones
[1,4,5] -> [(Operacion op  x y, [1,4,5])] -}
avanzar :: Estado -> [Paso]
avanzar estado = [(s, getEstados estado s) 
                  | s   <- getOperaciones (pares estado) operadores
                 ] -- fin de list comprehensions

{-Tengo una lista de pasos. [(Operacion op  x y, [1,4,5])] -> 
para cada uno de ellos:
Paso inicial    ->  (Operacion "suma" 0 0,[3,3]) 
0. Cada paso está compuesto de una operacion a la que conduce a un estado. paso = Operacion + estado
1. Obtengo el estado del paso.
2. Con ese estado a traves de avanzar obtenemos una lista de-}

-- solucionar :: [Paso] ->
--getEstadodelPaso (Operacion "suma" 1 2,[3,3])

getEstadodelPaso :: Paso -> Estado
getEstadodelPaso (_,estado) = estado

nextGeneration :: Paso -> [Paso]
nextGeneration estado = avanzar $ getEstadodelPaso estado

-- Podriamos tambien tener en cuenta que sea igual con cierto margen
isSolution :: Int -> Estado -> Bool
isSolution  solucion estado= elem solucion estado

-- haySolucion 11 [[1..5],[4..7],[4..11]]  == True
-- haySolucion 11 [[1..5],[4..7],[4..10]]  == False
-- haySolucion 12 [[]]                     == False
haySolucion :: Int -> Paso  -> Bool
haySolucion solucion paso =  isSolution solucion (getEstadodelPaso paso)

type Camino  = [Paso]     -- Un recorrido desde el inicio
type Caminos = [[Paso]]   -- Donde almacenamos todos los caminos
-- Cuando los estados que obtenemos son de longitud uno o hemos alcanzado una solucion
-- solucionar pasos caminos
-- reverse $ solucionar [[(Operacion "suma" 1 3,[4,5])]]
-- solucionar :: Caminos -> Caminos
-- para cada camino dentro de los caminos:
--    Obtenemos el último camino
--    Calculamos los nuevos estados mediante avanzar
--    Creamos nuevos caminos anexando los nuevos caminos a la lista de caminos
-- Tendremos que saber si hay alguna solucion 
-- solucionar [[(Operacion "suma" 3 4, [1,2,3])]] 5 [[]]
solucionar :: Caminos -> Int -> Caminos -> Int -> Caminos
solucionar caminos solucion soluciones n
 | n == 0                 = soluciones    -- condicion de salida por iteraciones
 | length soluciones /= 1 = soluciones    -- condicion cuando tenemos una solucion
 | otherwise              = solucionar next_generation solucion  (sols ++ soluciones)   (n-1)  --Natural rec
   where 
     next_generation =                                -- Para cada camino generamos el resto de caminos
        [ss :camino  |                                -- Los unimos al camino que ya tenemos
             camino <- caminos,                       -- para cada camino
             let ultimo = head camino,                -- primer estado es el ultimo
             ss <- nextGeneration ultimo]             -- Siguiente generacion, lista de pasos 
     sols           = 
        [ camino |                                    -- Si hay alguna solucion la podremos almacenar
              camino <- next_generation,              -- Para cada una de las nuevas generaciones
              let ultimo = head camino,               -- Extraemos la ultima solucion
              haySolucion solucion ultimo]            -- Vemos si es una solucion
      

solucion :: Estado -> Int -> Caminos
solucion estado solucion = 
   map reverse $ solucionar [[(Operacion "suma" 0 0,estado)]] solucion [[]] (length estado) 


