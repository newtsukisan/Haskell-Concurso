module Main where

import Propio              -- Solución propia
import Lib                 -- Solucion desarrollada en el libro
import System.Environment  -- Para manejarnos con la entrada-salida
   
-- obtenemos la solucion mediante una llamada del estilo
-- stack exec concurso-exe [1,3,7,10,25,50] 765
-- Algunas funciones para simplificar el proceso de mostrar la solucion

getEstado     :: String -> [Int]
getEstado str = read str :: [Int]

getObjetivo     :: String -> Int
getObjetivo str = read str :: Int


main :: IO ()
main = do args     <- getArgs
          putStrLn (show $ solucion (getEstado (args !! 0))   (getObjetivo (args !! 1)))
