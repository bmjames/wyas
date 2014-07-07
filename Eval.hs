module Eval where

import Data
import Parser

import Control.Monad (forever)

type LispFun = [LispVal] -> LispVal
type BinOp a = a -> a -> a

eval :: LispVal -> LispVal
eval val = case val of
  Atom _      -> val
  String _    -> val
  Number _    -> val
  Float _     -> val
  Bool _      -> val
  Character _ -> val

  List [Atom "quote", v] -> v
  List (Atom fun : args) -> apply fun $ map eval args

  List vs   -> List $ map eval vs
  Vector vs -> Vector $ fmap eval vs

  DottedList v1 v2 -> DottedList (map eval v1) (eval v2)

apply :: String -> LispFun
apply fun args = maybe (Bool False) ($ args) $ lookup fun primitives

primitives :: [(String, LispFun)]
primitives = [
    ("+", numericBinOp (+))
  , ("-", numericBinOp (-))
  , ("*", numericBinOp (*))
  , ("/", numericBinOp div)
  , ("mod", numericBinOp mod)
  , ("quotient", numericBinOp quot)
  , ("remainder", numericBinOp rem)

  , ("string?",  typeTest isString)
  , ("bool?",    typeTest isBool)
  , ("char?",    typeTest isChar)
  , ("number?",  typeTest isNumber)
  , ("integer?", typeTest isInteger)
  , ("list?",    typeTest isList)
  , ("pair?",    typeTest isPair)
  , ("vector?",  typeTest isVector)
  ]

numericBinOp :: BinOp Integer -> LispFun
numericBinOp op params = Number $ foldl1 op $ map unpackNum params where
  unpackNum (Number n) = n
  unpackNum _ = 0

typeTest :: (LispVal -> Bool) -> LispFun
typeTest f [v] = Bool $ f v

mergeEither :: Either a a -> a
mergeEither (Left a)  = a
mergeEither (Right a) = a

main :: IO ()
main = forever $ do
  putStr "> "
  l <- getLine
  putStrLn $ mergeEither $ fmap (show . eval) $ readExpr l
