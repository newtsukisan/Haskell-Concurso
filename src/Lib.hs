module Lib
  where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Definimos un tipo de dato operacion

data Op = Add | Sub | Mul | Div deriving (Show)

-- Definimos que operaciones son validas

valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True              -- Siempre podremos sumar varios numeros enteros
valid Sub x y = x > y             -- Para no tener numeros negativos.
valid Mul _ _ = True              -- Siempre podremos multiplicar dos enteros
valid Div x y = x `mod` y == 0    -- Solo si es un numero divisible del segundo

{-Una funcion para aplicar las operaciones-}
apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)  

{-Consideramos ahora una expresion-} 
data Expr = Val Int | App Op Expr Expr  deriving (Show)-- Podemos crear una expresion con el constructo App

{-Definimos ahora una funcio eval que devuelve una lista de los valores de una expresion-}
values :: Expr -> [Int]
values (Val n)     = [n]           -- Si solo tenemos un valor devolvemos una lista con el
values (App _ l r) = values l ++ values r
                          
eval :: Expr -> [Int]
eval (Val n)     = [n | n > 0]
eval (App o l r) = [apply o x y | x <- eval l,
                                  y <- eval r,
                                  valid o x y]    -- De forma recursiva obtenemos todos las
                                                  -- evaluaciones
 
{-subs nos sirve para obtener todas las subsecuencias de una lista-}
subs :: [a] -> [[a]]
subs []       = [[]]
subs (x : xs) = yss ++ map (x:) yss
                where yss = subs xs

{-Interleaves devuelve todas las posibles formas de insertar un nuevo elemento en una lista-}
interleave :: a -> [a] -> [[a]]
interleave x []       = [[x]]
interleave x (y : ys) = (x : y : ys) : map (y:) (interleave x ys)

{-Devuelve todas las alternativas de una lista que vienen dadas por todas las posibles 
formas de seleccionar cero o mas elementos en cualquier orden-}
choices :: [a] -> [[a]]
choices xs = concat (map perms (subs xs))


{-perms devuelve todas las permutaciones de una lista-}
perms :: [a] -> [[a]]
perms []       = [[]]                 -- Caso base
perms (x : xs) = concat (map (interleave x) (perms xs))
              
{-Solutions, Una expresion es una solucion para una lista dada de numeros y un objetivo si
la lista de valores en la expresion se elige desde la lista de numeros y la expresion evalua
correctamente para para dar el resultado
Solo tenemos una solucion si la expresion al evaluarla nos da el resultado y si 
los valores de esa expresion  estan en la lista de valores originales.
-}
solution :: Expr -> [Int] -> Int -> Bool
solution e ns n = elem (values e) (choices ns) && eval e == [n]
                                                   
e :: Expr

e = App Mul (App Add (Val 1) (Val 50)) (App Sub (Val 25) (Val 10))

isSolution = solution e [1,3,7,10,25,50] 765  

{-Solucion por fuerza bruta-}
split :: [a] -> [([a],[a])]
split []     = []
split [_]    = []
split (x:xs) = ([x],xs): [(x:ls,rs) | (ls,rs) <- split xs]

exprs :: [Int] -> [Expr]
exprs []  = []
exprs [n] = [Val n]
exprs ns   = [e | (ls,rs) <- split ns,
                  l <- exprs ls,
                  r <- exprs rs,
                  e <- combine l r]
combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]
ops     :: [Op]
ops = [Add,Sub,Mul,Div] 

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns,
                      e   <- exprs ns',
                      eval e == [n]]


--eval App Mul (Val 3) (App Sub (App Mul (Val 7) (App Sub (Val 50) (Val 10))) (Val 25))

