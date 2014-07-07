module Data where

import qualified Data.Vector as V

data LispVal = Atom       String
             | List       [LispVal]
             | DottedList [LispVal] LispVal
             | Vector     (V.Vector LispVal)
             | Number     Integer
             | Float      Double
             | String     String
             | Bool       Bool
             | Character  Char

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

  where
    unwordsVal = unwords . map showVal

instance Show LispVal where
  show = showVal
