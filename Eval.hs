module Eval where

import Data

import Prelude hiding (null)

import Control.Applicative ((<$>), (<*>), (<|>))
import Control.Monad.Error (ErrorT, runErrorT, throwError)
import Control.Monad.State (State, runState, get, put, gets, modify)

import Data.Foldable    (foldrM)
import Data.Traversable (traverse)

import qualified Data.Map as Map

type Env = Map.Map String LispVal

type Eval = ErrorT LispError (State Env)

type LispFun = [LispVal] -> Eval LispVal

nullEnv :: Env
nullEnv = Map.empty

runEval :: Env -> Eval a -> (ThrowsError a, Env)
runEval env = flip runState env . runErrorT

type BinOp a = a -> a -> a

eval :: LispVal -> Eval LispVal
eval val = case val of
  String _    -> return val
  Number _    -> return val
  Float _     -> return val
  Bool _      -> return val
  Character _ -> return val

  Atom ident  -> getVar ident

  List [Atom "quote", v] -> return v

  List [Atom "if", p, conseq, alt] -> do result <- eval p
                                         case result of
                                           Bool False -> eval alt
                                           _          -> eval conseq

  List (Atom "cond" : clauses) -> evalCond clauses

  List (Atom "case" : key : clauses) -> flip evalCase clauses =<< eval key

  List [Atom "define", Atom name, form] -> eval form >>= defineVar name

  List [Atom "lambda", List params, body] ->
    return $ Function (map showVal params) Nothing body

  List (fun : args) -> applyFun fun =<< traverse eval args

  Vector vs -> Vector <$> traverse eval vs

  DottedList v1 v2 -> DottedList <$> traverse eval v1 <*> eval v2

  badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm

defineVar :: String -> LispVal -> Eval LispVal
defineVar name value = do
  modify $ Map.insert name value
  return value

getVar :: String -> Eval LispVal
getVar name = findVar name >>= maybe (throwError $ UnboundVar name) return

findVar :: String -> Eval (Maybe LispVal)
findVar name = gets $ Map.lookup name

evalCond :: LispFun
evalCond [] = return $ Bool False
evalCond [List [Atom "else", v]] = eval v
evalCond (c:cs) = maybe (evalCond cs) return =<< evalClause =<< eval c
  where
    evalClause (List [p, v]) = do b <- unpackBool =<< eval p
                                  if b then Just <$> eval v else return Nothing
    evalClause badClause = throwError $ BadSpecialForm
                             "Invalid conditional clause" badClause

evalCase :: LispVal -> LispFun
evalCase _   []     = return $ Bool False
evalCase key (c:cs) = maybe (evalCase key cs) return =<< evalClause c
  where
    evalClause (List [List ds, v]) = traverse eval $
      if any (eqvInternal key) ds then Just v else Nothing
    evalClause badClause = throwError $ BadSpecialForm
                             "Invalid case clause" badClause

applyFun :: LispVal -> LispFun
applyFun fun args = case fun of
  Atom f -> lookupFun f >>= ($ args)
  val    -> applyUserFun val args

lookupFun :: String -> Eval LispFun
lookupFun name = do maybeVal <- findVar name
                    let maybePrimitive = lookup name primitives
                        maybeFun = (applyUserFun <$> maybeVal) <|> maybePrimitive
                    maybe (throwError $ UnboundVar name) return maybeFun

applyPrimitive :: String -> LispFun
applyPrimitive fun args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function" $ Atom fun)
    ($ args)
    (lookup fun primitives)

applyUserFun :: LispVal -> LispFun
applyUserFun fun args = case fun of
  Function params Nothing _ | length params /= length args ->
    throwError $ NumArgs (toInteger $ length params) args
  Function params vararg body -> do
    curEnv <- get
    mapM_ (uncurry defineVar) (zip params args)
    value <- eval body
    put curEnv
    return value
  notFun -> throwError $ NotFunction "Not a function" notFun

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
  , (">=", numBoolBinOp (>=))
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
  , ("string-append",  stringAppend)

  , ("car",  car)
  , ("cdr",  cdr)
  , ("cons", cons)
  , ("null?", null)

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

unpackNum :: LispVal -> Eval Integer
unpackNum (Number n) = return n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> Eval Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

unpackString :: LispVal -> Eval String
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

stringAppend :: LispFun
stringAppend = fmap String . foldrM append "" where
  append (String s) acc = return $ s ++ acc
  append notString  _   = throwError $ TypeMismatch "string" notString

boolBinOp :: (LispVal -> Eval a) -> (a -> a -> Bool) -> LispFun
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

null :: LispFun
null [List xs] = return $ Bool $ case xs of [] -> True
                                            _  -> False
null [notList] = throwError $ TypeMismatch "list" notList
null badArgs   = throwError $ NumArgs 1 badArgs

eqv :: LispFun
eqv [v1, v2] = return $ Bool $ eqvInternal v1 v2
eqv badArgs  = throwError $ NumArgs 2 badArgs

eqvInternal :: LispVal -> LispVal -> Bool
eqvInternal = (==)
