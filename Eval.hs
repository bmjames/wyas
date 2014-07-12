module Eval where

import Data
import Parser

import Control.Applicative ((<$>), (<*>))
import Control.Monad       (forever)
import Control.Monad.Error (throwError)

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

  List [Atom "if", p, conseq, alt] -> do result <- eval p
                                         case result of
                                           Bool False -> eval alt
                                           _          -> eval conseq

  List (Atom "cond" : clauses) -> evalCond clauses

  List (Atom fun : args) -> apply fun =<< traverse eval args

  Vector vs -> Vector <$> traverse eval vs

  DottedList v1 v2 -> DottedList <$> traverse eval v1 <*> eval v2

  badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

evalCond :: LispFun
evalCond [] = return $ Bool False
evalCond [List [Atom "else", v]] = eval v
evalCond (c:cs) = maybe (evalCond cs) return =<< evalClause c
  where
    evalClause (List [p, v]) = do b <- unpackBool =<< eval p
                                  if b then Just <$> eval v else return Nothing
    evalClause badClause = throwError $ BadSpecialForm
                             "Invalid conditional clause" badClause

apply :: String -> LispFun
apply fun args = maybe (throwError $ NotFunction "Unrecognized primitive function" fun) ($ args) $ lookup fun primitives

primitives :: [(String, LispFun)]
primitives = [
    ("+",         numericBinOp (+))
  , ("-",         numericBinOp (-))
  , ("*",         numericBinOp (*))
  , ("/",         numericBinOp div)
  , ("mod",       numericBinOp mod)
  , ("quotient",  numericBinOp quot)
  , ("remainder", numericBinOp rem)

  , ("=",  numBoolBinOp (==))
  , ("<",  numBoolBinOp (<))
  , (">",  numBoolBinOp (>))
  , ("/=", numBoolBinOp (/=))
  , (">=", numBoolBinOp (<=))
  , ("<=", numBoolBinOp (<=))

  , ("&&", boolBoolBinOp (&&))
  , ("||", boolBoolBinOp (||))

  , ("string=?",  strBoolBinOp (==))
  , ("string<?",  strBoolBinOp (<))
  , ("string>?",  strBoolBinOp (>))
  , ("string<=?", strBoolBinOp (<=))
  , ("string>=?", strBoolBinOp (>=))

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

  , ("car",  car)
  , ("cdr",  cdr)
  , ("cons", cons)

  , ("eq?", eqv)
  , ("eqv?", eqv)
  ]

  where
    numBoolBinOp  = boolBinOp unpackNum
    strBoolBinOp  = boolBinOp unpackString
    boolBoolBinOp = boolBinOp unpackBool

numericBinOp :: BinOp Integer -> LispFun
numericBinOp op [n1, n2] = fmap Number . op <$> unpackNum n1 <*> unpackNum n2
numericBinOp _ vs        = throwError $ NumArgs 2 vs

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackString :: LispVal -> ThrowsError String
unpackString (String s) = return s
unpackString notString = throwError $ TypeMismatch "string" notString

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

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> LispFun
boolBinOp f op [a1, a2] = fmap Bool . op <$> f a1 <*> f a2
boolBinOp _ _  as       = throwError $ NumArgs 2 as

car :: LispFun
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [badArg]             = throwError $ TypeMismatch "pair" badArg
car badArgs              = throwError $ NumArgs 1 badArgs

cdr :: LispFun
cdr [List (_:xs)]         = return $ List xs
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg]              = throwError $ TypeMismatch "pair" badArg
cdr badArgs               = throwError $ NumArgs 1 badArgs

cons :: LispFun
cons [x, List xs]          = return $ List (x:xs)
cons [x, DottedList xs x'] = return $ DottedList (x:xs) x'
cons [x1, x2]              = return $ DottedList [x1] x2
cons badArgs               = throwError $ NumArgs 2 badArgs

eqv :: LispFun
eqv [v1, v2] = return $ Bool $ v1 == v2
eqv badArgs  = throwError $ NumArgs 2 badArgs

main :: IO ()
main = forever $ do
  putStr "> "
  l <- getLine
  putStrLn $ either show show $ eval =<< readExpr l
