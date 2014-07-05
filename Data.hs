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
