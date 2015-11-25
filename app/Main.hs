module Main where

import Propio              -- Solución propia
import Countdown           -- Course solution
import Lib                 -- Solucion desarrollada en el libro
import System.Environment  -- Para manejarnos con la entrada-salida
   
-- obtenemos la solucion mediante una llamada del estilo
-- stack exec concurso-exe [1,3,7,10,25,50] 765
-- Para calcular los tiempos que podemos tardamos en encontrar las soluciones
-- time stack exec concurso-exe [1,3,7,10,25,50] 765 1

ejecutar       :: [String] -> String
ejecutar (x0:x1:x2:[])
   | x2 == "1" = sub_ejecuta solucion     x0 x1    -- Solucion propia
   | x2 == "2" = sub_ejecuta solutions'   x0 x1    -- Solucion del libro no mejorada
   | x2 == "3" = sub_ejecuta solutions''  x0 x1    -- Solucion del libro si mejorada
   | otherwise = "No se ha seleccionado uno de los dos  métodos disponibles"
      where 
         sub_ejecuta f arg0 arg1 = show $ f (getEstado (arg0))   (getObjetivo (arg1))
         getEstado     :: String -> [Int]       -- Para obtener el estado inicial
         getEstado str = read str :: [Int]      -- Lo convertimos en una lista de enteros
         getObjetivo     :: String -> Int       -- Para obtener el posible objetivo
         getObjetivo str = read str :: Int      -- Lo pasamos a un entero

main :: IO ()
main = do args     <- getArgs
          putStrLn (ejecutar args)
