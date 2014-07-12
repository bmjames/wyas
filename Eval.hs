module Eval where

import Data
import Parser

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (forever)
import Control.Monad.Error (throwError)

import Data.Bifunctor      (bimap)
import Data.Traversable    (traverse)

type LispFun = [LispVal] -> ThrowsError LispVal
type BinOp a = a -> a -> a

eval :: LispVal -> ThrowsError LispVal
eval val = case val of
  Atom _      -> return val
  String _    -> return val
  Number _    -> return val
  Float _     -> return val
  Bool _      -> return val
  Character _ -> return val

  List [Atom "quote", v] -> return v
  List (Atom fun : args) -> apply fun =<< traverse eval args

  Vector vs -> Vector <$> traverse eval vs

  DottedList v1 v2 -> DottedList <$> traverse eval v1 <*> eval v2

  badForm -> throwError $ BadSpecialForm badForm

apply :: String -> LispFun
apply fun args = maybe (throwError $ NotFunction "Unrecognized primitive function" fun) ($ args) $ lookup fun primitives

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
  , ("symbol?",  typeTest isSymbol)

  , ("symbol->string", symbolToString)
  , ("string->symbol", stringToSymbol)
  ]

numericBinOp :: BinOp Integer -> LispFun
numericBinOp op [n1, n2] = fmap Number . op <$> unpackNum n1 <*> unpackNum n2
  where
    unpackNum (Number n) = return n
    unpackNum notNum     = throwError $ TypeMismatch "number" notNum
numericBinOp _ vs        = throwError $ NumArgs 2 vs

typeTest :: (LispVal -> Bool) -> LispFun
typeTest f [v] = return . Bool $ f v
typeTest _ vs  = throwError $ NumArgs 2 vs

symbolToString :: LispFun
symbolToString [Atom s] = return $ String s
symbolToString [v]      = throwError $ TypeMismatch "symbol" v
symbolToString vs       = throwError $ NumArgs 2 vs

stringToSymbol :: LispFun
stringToSymbol [String s] = return $ Atom s
stringToSymbol [v]        = throwError $ TypeMismatch "string" v
stringToSymbol vs         = throwError $ NumArgs 2 vs

mergeEither :: Either a a -> a
mergeEither (Left a)  = a
mergeEither (Right a) = a

mergeMap :: (a -> c) -> (b -> c) -> Either a b -> c
mergeMap f g = mergeEither . bimap f g

main :: IO ()
main = forever $ do
  putStr "> "
  l <- getLine
  putStrLn $ mergeMap show show $ eval =<< readExpr l
