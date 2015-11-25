{-Con esta configuaracion es relativamente facil escribir una secuencia de test.-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Countdown

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Probando la funcionalidad basica de choices" 
              [
                testProperty "Definicion basica" prop1
              , testProperty "ver si funciona" prop2
              ]
        ]

prop1 (SmallIntList xs) = choices xs == (concat $ map perms $ subs xs) 
  

prop2 i = i == i + 0
  where types = (i :: Int) 

-- choices to be tested
-- one posible implementation is choices = concat . perms $ map subs

newtype SmallIntList = SmallIntList [Int] deriving (Eq,Show)

instance Arbitrary SmallIntList where
  arbitrary = sized $ \s -> do
                 n <- choose (0,s `min` 9)
                 xs <- vectorOf n (choose (-10000,10000))
                 return (SmallIntList xs)
  shrink (SmallIntList xs) = map SmallIntList (shrink xs) 
