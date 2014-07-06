module Eval where

import Data
import Parser

import Control.Monad (forever)

type LispFun = [LispVal] -> LispVal
type BinOp a = a -> a -> a

eval :: LispVal -> LispVal
eval val = case val of
  String _    -> val
  Number _    -> val
  Bool _      -> val
  Character _ -> val

  List [Atom "quote", v] -> v
  List (Atom fun : args) -> apply fun $ map eval args

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
  ]

numericBinOp :: BinOp Integer -> LispFun
numericBinOp op params = Number $ foldl1 op $ map unpackNum params where
  unpackNum (Number n) = n
  unpackNum _ = 0

mergeEither :: Either a a -> a
mergeEither (Left a)  = a
mergeEither (Right a) = a

main :: IO ()
main = forever $ do
  putStr "> "
  l <- getLine
  putStrLn $ mergeEither $ fmap (show . eval) $ readExpr l
