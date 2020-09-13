module Utils where

import Test.HUnit

testEqual :: (Eq a, Show a) => String -> a -> a -> Test
testEqual msg expected actual
  = TestCase $ assertEqual msg expected actual

testBool :: String -> Bool -> Test
testBool msg result
  = TestCase $ assertBool msg result
