module WYAS.Data where

import Control.Monad.Trans.Error (Error(..), ErrorT)
import Control.Monad.Morph       (hoist, generalize)

import Data.Functor.Identity (Identity)
import Data.Foldable         (foldMap)
import Data.Map              (Map)

import System.IO (Handle)
import Text.PrettyPrint.ANSI.Leijen (Doc)

import qualified Data.Vector as V

type ThrowsError = ErrorT LispError Identity
type IOThrowsError = ErrorT LispError IO

type LispFun = [LispVal] -> ThrowsError LispVal
type IOFun = [LispVal] -> IOThrowsError LispVal

type Env = Map String LispVal

data LispVal = Atom       String
             | List       [LispVal]
             | DottedList [LispVal] LispVal
             | Vector     (V.Vector LispVal)
             | Number     Integer
             | Float      Double
             | String     String
             | Bool       Bool
             | Character  Char
             | Function   Env [String] (Maybe String) LispVal
             | PrimFun    LispFun
             | IOFun      IOFun
             | Port       Handle

isString :: LispVal -> Bool
isString (String _) = True
isString _          = False

isBool :: LispVal -> Bool
isBool (Bool _) = True
isBool _        = False

isChar :: LispVal -> Bool
isChar (Character _) = True
isChar _             = False

isNumber :: LispVal -> Bool
isNumber (Number _) = True
isNumber (Float _)  = True
isNumber _          = False

isInteger :: LispVal -> Bool
isInteger (Number _) = True
isInteger _          = False

isList :: LispVal -> Bool
isList (List _) = True
isList _        = False

isPair :: LispVal -> Bool
isPair (DottedList _ _) = True
isPair _                = False

isVector :: LispVal -> Bool
isVector (Vector _) = True
isVector _          = False

isSymbol :: LispVal -> Bool
isSymbol (Atom _) = True
isSymbol _        = False

showVal :: LispVal -> String
showVal val = case val of
  Atom name       -> name
  List vs         -> "(" ++ unwordsVal vs ++ ")"
  DottedList vs v -> "(" ++ unwordsVal vs ++ " . " ++ showVal v ++ ")"
  Vector vs       -> "#(" ++ unwordsVal (V.toList vs) ++ ")"
  Number i        -> show i
  Float d         -> show d
  String s        -> show s
  Bool True       -> "#t"
  Bool False      -> "#f"
  Character ' '   -> "#\\space"
  Character '\t'  -> "#\\tab"
  Character '\n'  -> "#\\newline"
  Character c     -> ['#', '\\', c]

  Function _ params vararg  _ ->
    "(lambda (" ++ unwords params ++ foldMap (" . " ++) vararg ++ ") ...)"

  PrimFun _     -> "<primitive>"
  IOFun   _     -> "<i/o primitive>"
  Port    _     -> "<i/o port>"

  where
    unwordsVal = unwords . map showVal

instance Show LispVal where
  show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | ParseError Doc
               | BadSpecialForm String LispVal
               | NotFunction String LispVal
               | UnboundVar String
               | Default String
               deriving Show

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = hoist generalize

instance Error LispError where
  noMsg = Default "An error has occurred"
  strMsg = Default
