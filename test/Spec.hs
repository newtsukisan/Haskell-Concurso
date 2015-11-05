{-Con esta configuaracion es relativamente facil escribir una secuencia de test.-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Sorting Group 1" [
                testProperty "prop1" prop1,
                testProperty "prop2" prop2
           ]
      ]

prop1 b = foldr (&&) True b == all (==True) b
  where types = (b :: [Bool])

prop2 i = i == i + 3
  where types = (i :: Int)
