{-Con esta configuaracion es relativamente facil escribir una secuencia de test.-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Probando la funcionalidad basica de choices" 
              [
                testProperty "ver si funciona" prop1
              , testProperty "ver si funciona" prop2
              ]
        ]

prop1 b = foldr (&&) True b == all (==True) b
  where types = (b :: [Bool])

prop2 i = i == i + 0
  where types = (i :: Int) 

-- choices to be tested
-- one posible implementation is choices = concat . perms $ map subs 
