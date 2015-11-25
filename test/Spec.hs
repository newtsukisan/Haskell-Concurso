{-Con esta configuaracion es relativamente facil escribir una secuencia de test.-}
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Data.List

import Test.QuickCheck
import Countdown
import Propio

main :: IO ()
main = defaultMain tests

tests = [
        testGroup "Testing implementations for exercises" 
              [
                testProperty "Using known definition"           prop1
              , testProperty "Simple Checking"                  prop2
              , testProperty "isChoice must be true - 1"        prop3
              , testProperty "isChoice must be true - 2"        prop4
              , testProperty "removing known element 1"         prop5
              , testProperty "removing known element 2"         prop6
              ]
        ]
-- Comparing with one possible implemetation
prop1 (SmallIntList xs) = choices xs == (concat $ map perms $ subs xs) 
-- Checking trivial for security
prop2 i = i == i + 0
  where types = (i :: Int)  
-- isChoice true cases 
prop3 xs             = 
   not (null xs) ==>   -- not empty lists
    classify (length xs < 2) "trivial" $  isChoice (tail xs) xs  -- must be true
    where types = (xs::[Int])
prop4 xs             = 
   not (null xs) ==>   -- not empty lists
    classify (length xs < 2) "trivial" $  isChoice [(head xs)] xs -- must be true                            
    where types = (xs::[Int])

prop5 xs             = 
   not (null xs) ==>   -- not empty lists
    classify (length xs < 2) "trivial" $  isChoice (removeone elemento xs) xs-- must be inside                            
    where types    = (xs::[Int])
          elemento = last (sort xs)

prop6 xs             = 
   not (null xs) ==>   -- not empty lists
    classify (length xs < 2) "trivial" $  size_after + 1  == size_before  -- must be inside                            
    where types       = (xs::[Int])
          elemento    = head (sort xs)                 -- after sorting take anyone
          size_after  = length (removeone elemento xs)
          size_before = length xs

-- choices to be tested
-- only certain size for list of integer when testing choices to avoid big time calcutations
newtype SmallIntList = SmallIntList [Int] deriving (Eq,Show)

instance Arbitrary SmallIntList where
  arbitrary = sized $ \s -> do
                 n <- choose (0,s `min` 7)
                 xs <- vectorOf n (choose (-10000,10000))
                 return (SmallIntList xs)
  shrink (SmallIntList xs) = map SmallIntList (shrink xs) 
