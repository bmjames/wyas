module Parser where

import Control.Applicative hiding (many, (<|>))
import Control.Monad      (forever)

import Data.Char    (digitToInt, toLower, toUpper)
import Data.Functor (($>))
import Data.Traversable (traverse)

import Numeric (readOct, readHex, readInt, readFloat)

import Text.ParserCombinators.Parsec


data LispVal = Atom       String
             | List       [LispVal]
             | DottedList [LispVal] LispVal
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
parseNumber = fmap Number $ (read <$> many1 digit) <|> char '#' *> parseNumber'
  where
    parseNumber' = char 'o' *> parseOct
               <|> char 'x' *> parseHex
               <|> char 'b' *> parseBin

parseBin :: Parser Integer
parseBin = fst . head . readBin <$> many1 (oneOf "01") where
  readBin = readInt 2 (`elem` "01") digitToInt

parseOct :: Parser Integer
parseOct = fst . head . readOct <$> many1 octDigit

parseHex :: Parser Integer
parseHex = fst . head . readHex <$> many1 hexDigit

parseFloat :: Parser LispVal
parseFloat = Float <$> parseFloat'
  where
    parseFloat' = string "#d" *> (fst . head . readFloat <$> float')
    float' = do int  <- many1 digit
                _    <- char '.'
                frac <- many1 digit
                return $ int ++ "." ++ frac

parseList :: Parser LispVal
parseList = List <$> parseExpr `sepBy` spaces

parseExpr :: Parser LispVal
parseExpr =
      try parseNumber
  <|> try parseFloat
  <|> parseChar
  <|> parseAtom
  <|> parseString
  <|> parseQuoted
  <|> (char '(' *> (try parseList <|> parseDottedList) <* char ')')

parseDottedList :: Parser LispVal
parseDottedList = DottedList
  <$> parseExpr `endBy` spaces
  <*> (char '.' *> spaces *> parseExpr)

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  e <- parseExpr
  return $ List [Atom "quote", e]

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right v  -> "Found value: " ++ show v

main :: IO ()
main = forever $ getLine >>= putStrLn . readExpr
