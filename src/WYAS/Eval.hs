module WYAS.Eval where

import WYAS.Data
import WYAS.Parser (readExpr, readExprList)

import Prelude hiding (null, error)

import Control.Applicative    ((<$>), (<*>))

import Control.Monad.Trans.State  (StateT, runStateT, get, gets, put, modify)
import Control.Monad.Trans.Error  (ErrorT, runErrorT, throwError)
import Control.Monad.Trans.Class  (lift)
import Control.Monad.IO.Class     (liftIO)

import Control.Monad.Morph   (hoist, generalize)
import Data.Functor          ((<$))
import Data.Functor.Identity (Identity)
import Data.Maybe            (listToMaybe, fromMaybe)

import Data.Foldable    (foldrM, traverse_)
import Data.Traversable (traverse)

import System.IO

import qualified Data.Map     as Map
import qualified Data.Text.IO as Text
import qualified Data.Vector  as Vector

type EvalT f a = StateT Env (ErrorT LispError f) a
type EvalIO a = EvalT IO a
type Eval a = EvalT Identity a

error :: Monad f => LispError -> EvalT f a
error = lift . throwError

hoistEval :: Monad f => Eval a -> EvalT f a
hoistEval = hoist (hoist generalize)

runEval :: Monad f => Env -> EvalT f a -> f (Either LispError (a, Env))
runEval env fa = runErrorT $ runStateT fa env

type BinOp a = a -> a -> a

eval :: LispVal -> EvalIO LispVal
eval val = case val of
  String _    -> return val
  Number _    -> return val
  Float _     -> return val
  Bool _      -> return val
  Character _ -> return val
  PrimFun _   -> return val
  IOFun _     -> return val
  Port _      -> return val

  Atom ident  -> hoistEval $ getVar ident

  List [Atom "quote", v] -> return v

  List [Atom "if", p, conseq, alt] -> do result <- eval p
                                         case result of
                                           Bool False -> eval alt
                                           _          -> eval conseq

  List (Atom "cond" : clauses) -> evalCond clauses

  List (Atom "case" : key : clauses) -> flip evalCase clauses =<< eval key

  List [Atom "define", Atom name, form] -> eval form >>= hoistEval . defineVar name
  List [Atom "define", List (Atom fun : params), body] ->
    hoistEval $ makeFun Nothing params body >>= defineVar fun
  List [Atom "define", DottedList (Atom fun : params) (Atom varargs), body] ->
    hoistEval $ makeFun (Just varargs) params body >>= defineVar fun

  List [Atom "lambda", List params, body] ->
    hoistEval $ makeFun Nothing params body
  List [Atom "lambda", DottedList params (Atom varargs), body] ->
    hoistEval $ makeFun (Just varargs) params body

  List [Atom "load", String filename] ->
    lift (load filename) >>= traverse eval >>= return . fromMaybe (List []) . listToMaybe

  List (fun : args) -> do f  <- eval fun
                          as <- traverse eval args
                          applyFun f as

  Vector vs -> Vector <$> traverse eval vs

  DottedList v1 v2 -> DottedList <$> traverse eval v1 <*> eval v2

  badForm -> error $ BadSpecialForm "Unrecognized special form" badForm

makeFun :: Maybe String -> [LispVal] -> LispVal -> Eval LispVal
makeFun varargs params body = do
  env <- get
  return $ Function env (map showVal params) varargs body

defineVar :: String -> LispVal -> Eval LispVal
defineVar name value = do
  modify $ Map.insert name value
  return value

getVar :: String -> Eval LispVal
getVar name = findVar name >>= maybe (error $ UnboundVar name) return

findVar :: String -> Eval (Maybe LispVal)
findVar name = gets $ Map.lookup name

evalCond :: [LispVal] -> EvalIO LispVal
evalCond [] = return $ Bool False
evalCond [List [Atom "else", v]] = eval v
evalCond (c:cs) = maybe (evalCond cs) return =<< evalClause =<< eval c
  where
    evalClause (List [p, v]) = do b <- hoistEval . lift . unpackBool =<< eval p
                                  if b then Just <$> eval v else return Nothing
    evalClause badClause = error $ BadSpecialForm
                             "Invalid conditional clause" badClause

evalCase :: LispVal -> [LispVal] -> EvalIO LispVal
evalCase _   []     = return $ Bool False
evalCase key (c:cs) = maybe (evalCase key cs) return =<< evalClause c
  where
    evalClause (List [List ds, v]) = traverse eval $
      if any (eqvInternal key) ds then Just v else Nothing
    evalClause badClause = error $ BadSpecialForm
                             "Invalid case clause" badClause

applyFun :: LispVal -> [LispVal] -> EvalIO LispVal
applyFun fun args = case fun of
  PrimFun f -> hoistEval $ lift $ f args
  IOFun f   -> lift $ f args
  val       -> applyUserFun val args

applyUserFun :: LispVal -> [LispVal] -> EvalIO LispVal
applyUserFun fun args = case fun of
  Function _ params Nothing _ | length params /= length args ->
    error $ NumArgs (toInteger $ length params) args
  Function env params vararg body -> do
    curEnv <- get
    modify (Map.union env)
    traverse_ (hoistEval . uncurry defineVar) (zip params args)
    let varargs = List $ drop (length params) args
    traverse_ (hoistEval . flip defineVar varargs) vararg
    value <- eval body
    put curEnv
    return value
  notFun -> error $ NotFunction "Not a function" notFun

primitiveBindings :: Env
primitiveBindings = Map.union (fmap PrimFun primitives) (fmap IOFun ioPrimitives)

ioPrimitives :: Map.Map String IOFun
ioPrimitives = Map.fromList [
    ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("read-contents", readContents)
  , ("read-all", readAll)
  ]

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode
makePort _    [badArg]          = throwError $ TypeMismatch "string" badArg
makePort _    badArgs           = throwError $ NumArgs 1 badArgs

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ Bool True <$ hClose port
closePort _           = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = liftIO (Text.hGetLine port) >>= liftThrows . readExpr
readProc [badArg]    = throwError $ TypeMismatch "string" badArg
readProc badArgs     = throwError $ NumArgs 1 badArgs

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ Bool True <$ hPrint port obj
writeProc _                = return $ Bool False

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = fmap String $ liftIO $ readFile filename
readContents [badArg]          = throwError $ TypeMismatch "string" badArg
readContents badArgs           = throwError $ NumArgs 1 badArgs

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (Text.readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = fmap List $ load filename
readAll [badArg]          = throwError $ TypeMismatch "string" badArg
readAll badArgs           = throwError $ NumArgs 1 badArgs

primitives :: Map.Map String LispFun
primitives = Map.fromList [
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
typeTest _ vs  = throwError $ NumArgs 1 vs

symbolToString :: LispFun
symbolToString [Atom s] = return $ String s
symbolToString [v]      = throwError $ TypeMismatch "symbol" v
symbolToString vs       = throwError $ NumArgs 1 vs

stringToSymbol :: LispFun
stringToSymbol [String s] = return $ Atom s
stringToSymbol [v]        = throwError $ TypeMismatch "string" v
stringToSymbol vs         = throwError $ NumArgs 1 vs

stringAppend :: LispFun
stringAppend = fmap String . foldrM append "" where
  append (String s) acc = return $ s ++ acc
  append notString  _   = throwError $ TypeMismatch "string" notString

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

null :: LispFun
null [List xs] = return $ Bool $ case xs of [] -> True
                                            _  -> False
null [notList] = throwError $ TypeMismatch "list" notList
null badArgs   = throwError $ NumArgs 1 badArgs

eqv :: LispFun
eqv [v1, v2] = return $ Bool $ eqvInternal v1 v2
eqv badArgs  = throwError $ NumArgs 2 badArgs

eqvInternal :: LispVal -> LispVal -> Bool
eqvInternal (Atom s1) (Atom s2) = s1 == s2
eqvInternal (List xs) (List ys) | length xs == length ys = and $ zipWith eqvInternal xs ys
eqvInternal (DottedList xs x) (DottedList ys y) = eqvInternal (List (x:xs)) (List (y:ys))
eqvInternal (Vector xs) (Vector ys) = eqvInternal (List $ Vector.toList xs) (List $ Vector.toList ys)
eqvInternal (Number i) (Number j) = i == j
eqvInternal (Float i) (Float j) = i == j
eqvInternal (Bool b1) (Bool b2) = b1 == b2
eqvInternal (Character c1) (Character c2) = c1 == c2
eqvInternal (Port h1) (Port h2) = h1 == h2
eqvInternal _ _ = False
