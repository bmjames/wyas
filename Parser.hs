{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data

import Control.Applicative
import Control.Monad.Error (throwError)

import Data.Attoparsec.Text
import Data.Char        (digitToInt, toLower, toUpper)
import Data.Traversable (traverse)

import Numeric          (readOct, readHex, readInt, readFloat)

import qualified Data.Vector as V


symbol :: Parser Char
symbol = satisfy (inClass "-!#$%&|*+/:<=>?@^_~") <?> "symbol"

parseString :: Parser LispVal
parseString = String <$> (char '"' *> many char' <* char '"') <?> "string"
  where
    char' = try (char '\\' *> escapedChar) <|> notChar '"'
    escapedChar =     ('"'  <$ char '"')
                  <|> ('\n' <$ char 'n')
                  <|> ('\r' <$ char 'r')
                  <|> ('\t' <$ char 't')
                  <|> ('\\' <$ char '\\')

parseChar :: Parser LispVal
parseChar = Character <$> char' <?> "character"
  where
    char'     = string "#\\" *> (try namedChar <|> anyChar <|> pure ' ')
    namedChar =     (' '  <$ iString "space")
                <|> ('\n' <$ iString "newline")
                <|> ('\t' <$ iString "tab")
    iString :: String -> Parser String
    iString   = traverse iChar
    iChar c   = char (toLower c) <|> char (toUpper c)

parseAtom :: Parser LispVal
parseAtom = atom <?> "symbol" where
  atom = do first <- letter <|> symbol
            rest  <- many (letter <|> digit <|> symbol)
            return $ case first:rest of
              "#t" -> Bool True
              "#f" -> Bool False
              sym  -> Atom sym

parseNumber :: Parser LispVal
parseNumber = (parseInt <|> char '#' *> parseNumber') <?> "number"
  where
    parseInt     = Number . read <$> many1 digit
    parseNumber' = char 'o' *> parseOct
               <|> char 'x' *> parseHex
               <|> char 'b' *> parseBin
               <|> char 'd' *> parseFloat

parseBin :: Parser LispVal
parseBin = parseBin' <?> "binary" where
  parseBin' = fmap Number $ fst . head . readBin <$> many1 binDigit
  binDigit  = satisfy (\c -> c == '0' || c == '1') <?> "'0' or '1'"
  readBin   = readInt 2 (`elem` "01") digitToInt

parseOct :: Parser LispVal
parseOct = fmap Number $ fst . head . readOct <$> many1 octDigit
  where octDigit = satisfy (inClass "0-7") <?> "'0'..'7'"

parseHex :: Parser LispVal
parseHex = fmap Number $ fst . head . readHex <$> many1 hexDigit
  where hexDigit = satisfy (inClass "0-9a-f") <?> "'0'..'f'"

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
  es  <- exprs
  val <- (dottedList es <?> "dotted list") <|> pure (List es)
  char ')'
  return val

  where
    dottedList es = DottedList es <$> (char '.' *> skipSpace *> parseExpr)

parseVector :: Parser LispVal
parseVector = Vector . V.fromList <$> vec where
  vec = (string "#(" *> exprs <* char ')') <?> "vector"

exprs :: Parser [LispVal]
exprs = skipSpace *> (parseExpr `endBy` skipSpace) <?> "[expr..]" where
  endBy p sep = many (p <* sep)

parseExpr :: Parser LispVal
parseExpr =
      try parseNumber
  <|> try parseChar
  <|> try parseVector
  <|> parseAtom
  <|> parseString
  <|> parseQuoted
  <|> parseListOrPairs
