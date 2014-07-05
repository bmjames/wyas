module Parser where

import Control.Applicative hiding (many, (<|>))
import Control.Monad      (forever)

import Data.Char    (digitToInt, toLower, toUpper)
import Data.Functor (($>))
import Data.Traversable (traverse)

import Numeric (readOct, readHex, readInt, readFloat)

import Text.ParserCombinators.Parsec

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
             deriving Show

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

parseString :: Parser LispVal
parseString = String <$> (char '"' *> many char' <* char '"')
  where
    char' = try (char '\\' *> escapedChar) <|> noneOf "\""
    escapedChar =     (char '"' $> '"')
                  <|> (char 'n' $> '\n')
                  <|> (char 'r' $> '\r')
                  <|> (char 't' $> '\t')
                  <|> (char '\\' $> '\\')

parseChar :: Parser LispVal
parseChar = Character <$> char'
  where
    char'     = string "#\\" *> (try namedChar <|> anyChar <|> pure ' ')
    namedChar =     (iString "space"   $> ' ')
                <|> (iString "newline" $> '\n')
                <|> (iString "tab"     $> '\t')
    iString   = traverse iChar
    iChar c   = char (toLower c) <|> char (toUpper c)

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               return $ case first:rest of
                 "#t" -> Bool True
                 "#f" -> Bool False
                 atom -> Atom atom

parseNumber :: Parser LispVal
parseNumber = parseInt <|> char '#' *> parseNumber'
  where
    parseInt     = Number . read <$> many1 digit
    parseNumber' = char 'o' *> parseOct
               <|> char 'x' *> parseHex
               <|> char 'b' *> parseBin
               <|> char 'd' *> parseFloat

parseBin :: Parser LispVal
parseBin = fmap Number $ fst . head . readBin <$> many1 (oneOf "01") where
  readBin = readInt 2 (`elem` "01") digitToInt

parseOct :: Parser LispVal
parseOct = fmap Number $ fst . head . readOct <$> many1 octDigit

parseHex :: Parser LispVal
parseHex = fmap Number $ fst . head . readHex <$> many1 hexDigit

parseFloat :: Parser LispVal
parseFloat = Float <$> fst . head . readFloat <$> float' where
  float' = do int  <- many1 digit
              _    <- char '.'
              frac <- many1 digit
              return $ int ++ "." ++ frac

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  e <- parseExpr
  return $ List [Atom "quote", e]

parseListOrPairs :: Parser LispVal
parseListOrPairs = do
  char '('
  exprs <- parseExpr `endBy` spaces
  val   <- (DottedList exprs <$> (char '.' *> spaces *> parseExpr))
       <|> pure (List exprs)
  char ')'
  return val

parseVector :: Parser LispVal
parseVector = fmap (Vector . V.fromList) $
  string "#(" *> parseExpr `sepBy` spaces <* char ')'

parseExpr :: Parser LispVal
parseExpr =
      try parseNumber
  <|> try parseChar
  <|> try parseVector
  <|> parseAtom
  <|> parseString
  <|> parseQuoted
  <|> parseListOrPairs

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right v  -> "Found value: " ++ show v

main :: IO ()
main = forever $ getLine >>= putStrLn . readExpr
