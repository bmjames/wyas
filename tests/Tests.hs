module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

main :: IO ()
main = defaultMain [ testCase "hello world" helloWorld ]

helloWorld :: Assertion
helloWorld = assertEqual "True" True True
