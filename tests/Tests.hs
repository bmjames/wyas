module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import WYAS.Eval                 (eqvInternal)
import WYAS.Parser
import WYAS.Data
import qualified Data.Vector as V

main :: IO ()
main = defaultMain parserTests

parserTests =
  [ testCase "atom" atom
  , testCase "hex literal" hexLit
  , testCase "binary literal" binLit
  , testCase "oct literal" octLit
  , testCase "boolean" bool
  , testCase "vector" vector
  , testCase "string" string
  , testCase "quoted" quoted
  , testCase "list" list
  , testCase "pair" pair
  ]

atom   = testReadExpr "foobar"     $ Atom "foobar"
hexLit = testReadExpr "#xdeadbeef" $ Number 3735928559
binLit = testReadExpr "#b10101"    $ Number 21
octLit = testReadExpr "#o644"      $ Number 420
vector = testReadExpr "#(a b)"     $ Vector $ V.fromList [Atom "a", Atom "b"]
string = testReadExpr "\"foobar\"" $ String "foobar"
quoted = testReadExpr "'(a b)"     $ List [Atom "quote", List [Atom "a", Atom "b"]]
list   = testReadExpr "(a b c)"    $ List [Atom "a", Atom "b", Atom "c"]
pair   = testReadExpr "(a . b)"    $ DottedList [Atom "a"] (Atom "b")
bool   = testReadExpr "(#t #f)"    $ List [Bool True, Bool False]

testReadExpr :: String -> LispVal -> Assertion
testReadExpr exp val = case runThrowsError $ readExpr "test" exp of
  Left  e -> assertString $ "Parse error: " ++ show e
  Right v -> let msg = show v ++ " is not equivalent to " ++ show val
             in assertBool msg $ eqvInternal v val
