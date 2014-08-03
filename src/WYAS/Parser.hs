{-# LANGUAGE OverloadedStrings #-}

module WYAS.Parser where

import Prelude hiding (takeWhile)

import WYAS.Data

import Control.Applicative
import Control.Monad.Trans.Error (throwError)

import Data.Attoparsec.Text
import Data.Char        (digitToInt, toLower, toUpper)
import Data.Traversable (traverse)
import Data.Text        (Text)

import Numeric          (readOct, readHex, readInt, readFloat)

import qualified Data.Vector as V


symbol :: Parser Char
symbol = satisfy (inClass "-!#$%&|*+/:<=>?@^_~") <?> "symbol"

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
parseChar = Character <$> char' <?> "character"
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
  char '(' *> skipSpaceAndComment
  es  <- exprs
  val <- (dottedList es <?> "dotted list") <|> pure (List es)
  skipSpaceAndComment *> char ')'
  return val

  where
    dottedList es = DottedList es <$> (char '.' *> skipSpaceAndComment *> parseExpr)

parseVector :: Parser LispVal
parseVector = Vector . V.fromList <$> vec where
  vec = (string "#(" *> exprs <* char ')') <?> "vector"

exprs :: Parser [LispVal]
exprs = parseExpr `sepBy` skipSpaceAndComment <?> "[expr..]"

endBy :: Parser a -> Parser b -> Parser [a]
endBy p sep = many (p <* sep)

skipComment :: Parser ()
skipComment = string ";;" *> takeWhile (/= '\n') *> pure ()

skipSpaceAndComment :: Parser ()
skipSpaceAndComment = skipSpace *> option () (skipComment *> skipSpaceAndComment)

parseExpr :: Parser LispVal
parseExpr = parseNumber
            <|> parseChar
            <|> parseVector
            <|> parseAtom
            <|> parseString
            <|> parseQuoted
            <|> parseListOrPairs

readOrThrow :: Parser a -> Text -> ThrowsError a
readOrThrow parser =
  either (throwError . Parser) return . parseOnly parser

readExpr :: Text -> ThrowsError LispVal
readExpr = readOrThrow (skipSpaceAndComment *> parseExpr)

readExprList :: Text -> ThrowsError [LispVal]
readExprList = readOrThrow (skipSpaceAndComment *> sepBy parseExpr skipSpaceAndComment)
