{-# LANGUAGE OverloadedStrings #-}

module WYAS.Parser where

import Prelude hiding (takeWhile)

import WYAS.Data

import Control.Applicative
import Control.Monad.Trans.Error (throwError, runErrorT)

import Text.Trifecta hiding (parseString, symbol)
import Text.Trifecta.Delta (Delta(Columns))
import qualified Text.Trifecta as Trifecta

import Data.Char        (digitToInt, toLower, toUpper)
import Data.Traversable (traverse)
import Data.Functor     (void)
import Data.Functor.Identity (runIdentity)

import Numeric          (readOct, readHex, readInt, readFloat)

import qualified Data.Vector as V


symbol :: Parser Char
symbol = oneOf "-!#$%&|*+/:<=>?@^_~"

parseString :: Parser LispVal
parseString = String <$> (char '"' *> many char' <* char '"') <?> "string"
  where
    char' = (char '\\' *> escapedChar) <|> notChar '"'
    escapedChar =     ('"'  <$ char '"')
                  <|> ('\n' <$ char 'n')
                  <|> ('\r' <$ char 'r')
                  <|> ('\t' <$ char 't')
                  <|> ('\\' <$ char '\\')

parseChar :: Parser LispVal
parseChar = Character <$> char'
  where
    char'     = string "#\\" *> (namedChar <|> anyChar <|> pure ' ')
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
    parseInt     = Number . read <$> some digit
    parseNumber' = char 'o' *> parseOct
               <|> char 'x' *> parseHex
               <|> char 'b' *> parseBin
               <|> char 'd' *> parseFloat

parseBin :: Parser LispVal
parseBin = parseBin' <?> "binary" where
  parseBin' = fmap Number $ fst . head . readBin <$> some binDigit
  binDigit  = satisfy (\c -> c == '0' || c == '1') <?> "'0' or '1'"
  readBin   = readInt 2 (`elem` "01") digitToInt

parseOct :: Parser LispVal
parseOct = fmap Number $ fst . head . readOct <$> some octDigit
  where octDigit = oneOf ['0'..'7']

parseHex :: Parser LispVal
parseHex = fmap Number $ fst . head . readHex <$> some hexDigit
  where hexDigit = oneOf $ ['0'..'9'] ++ ['a'..'f']

parseFloat :: Parser LispVal
parseFloat = Float <$> fst . head . readFloat <$> float' where
  float' = do int  <- some digit
              _    <- char '.'
              frac <- some digit
              return $ int ++ "." ++ frac

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  e <- parseExpr
  return $ List [Atom "quote", e]

parseListOrPairs :: Parser LispVal
parseListOrPairs = do
  char '(' *> skipSpaceAndComment
  es  <- exprs
  val <- (dottedList es <?> "dotted list") <|> pure (List es)
  _   <- char ')'
  return val

  where
    dottedList es = DottedList es <$> (char '.' *> skipSpaceAndComment *> parseExpr)

parseVector :: Parser LispVal
parseVector = Vector . V.fromList <$> vec where
  vec = (string "#(" *> exprs <* char ')') <?> "vector"

exprs :: Parser [LispVal]
exprs = parseExpr `endBy` skipSpaceAndComment <?> "expr..."

skipComment :: Parser ()
skipComment = (string ";;" *> many (noneOf ['\n']) *> pure ()) <?> "comment"

skipSpace :: Parser ()
skipSpace = void $ many $ oneOf " \n\t\r"

skipSpaceAndComment :: Parser ()
skipSpaceAndComment = skipSpace *> option () (skipComment *> skipSpaceAndComment)

parseExpr :: Parser LispVal
parseExpr = (try parseNumber <?> "number")
            <|> (try parseChar <?> "char")
            <|> parseVector
            <|> parseAtom
            <|> parseString
            <|> parseQuoted
            <|> parseListOrPairs

readOrThrow :: Parser a -> String -> ThrowsError a
readOrThrow parser s =
  case Trifecta.parseString parser (Columns 0 0) s of
    Success a -> return a
    Failure d -> throwError $ ParseError d

readExpr :: String -> ThrowsError LispVal
readExpr = readOrThrow (skipSpaceAndComment *> parseExpr)

readExprList :: String -> ThrowsError [LispVal]
readExprList = readOrThrow (skipSpaceAndComment *> sepBy parseExpr skipSpaceAndComment)
