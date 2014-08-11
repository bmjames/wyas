module WYAS.Tests where

import Test.HUnit

import qualified Distribution.TestSuite as C
import qualified Distribution.TestSuite.HUnit as H

tests :: IO [C.Test]
tests = return $ map (uncurry H.test) testCases

testCases :: [(String, Test)]
testCases = [("Truth tests", truthTest)]

truthTest :: Test
truthTest = TestCase $ assertEqual "True" True True
