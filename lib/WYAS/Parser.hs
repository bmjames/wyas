{-# LANGUAGE OverloadedStrings #-}

module WYAS.Parser where

import Prelude hiding (takeWhile)

import WYAS.Data

import Control.Applicative
import Control.Monad.Trans.Error (throwError)

import Text.Trifecta hiding (parseString, symbol)
import Text.Trifecta.Delta (Delta(Directed))
import qualified Text.Trifecta as Trifecta

import Data.Char             (digitToInt, toLower, toUpper)
import Data.Functor          (void)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Vector as V

import Numeric          (readOct, readHex, readInt, readFloat)

symbol :: Parser Char
symbol = oneOf "-!#$%&|*+/:<=>?@^_~"

parseString :: Parser LispVal
parseString = String <$> (char '"' *> many char' <* char '"')
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
parseAtom =
  do first <- letter <|> symbol
     rest  <- many (letter <|> digit <|> symbol)
     return $ case first:rest of
       "#t" -> Bool True
       "#f" -> Bool False
       sym  -> Atom sym

parseNumber :: Parser LispVal
parseNumber = parseInt <|> char '#' *> parseNumber'
  where
    parseInt     = Number . read <$> some digit
    parseNumber' = char 'o' *> parseOct
               <|> char 'x' *> parseHex
               <|> char 'b' *> parseBin
               <|> char 'd' *> parseFloat

parseBin :: Parser LispVal
parseBin = parseBin' <?> "binary" where
  parseBin' = Number <$> fst . head . readBin <$> some binDigit
  binDigit  = satisfy (\c -> c == '0' || c == '1') <?> "'0' or '1'"
  readBin   = readInt 2 (`elem` ['0','1']) digitToInt

parseOct :: Parser LispVal
parseOct = Number <$> fst . head . readOct <$> some octDigit

parseHex :: Parser LispVal
parseHex = Number <$> fst . head . readHex <$> some hexDigit

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
parseListOrPairs =
  do
    char '(' *> skipSpaceAndComment
    es  <- exprs
    val <- (dottedList es <?> "dotted list") <|> pure (List es)
    _   <- char ')'
    return val

  where
    dottedList es = DottedList es <$> (char '.' *> skipSpaceAndComment *> parseExpr)

parseVector :: Parser LispVal
parseVector = Vector . V.fromList <$> (string "#(" *> exprs <* char ')')

exprs :: Parser [LispVal]
exprs = (parseExpr `endBy` skipSpaceAndComment) <?> "expr..."

skipComment :: Parser ()
skipComment = (string ";;" *> many (notChar '\n') *> pure ()) <?> "comment"

skipSpace :: Parser ()
skipSpace = void $ many $ oneOf " \n\t\r"

skipSpaceAndComment :: Parser ()
skipSpaceAndComment = skipSpace *> option () (skipComment *> skipSpaceAndComment)

parseExpr :: Parser LispVal
parseExpr = (try parseNumber <?> "number")
            <|> (try parseChar <?> "char")
            <|> parseVector
            <|> (parseAtom <?> "atom")
            <|> (parseString <?> "string")
            <|> parseQuoted
            <|> parseListOrPairs

readOrThrow :: Parser a -> FilePath -> String -> ThrowsError a
readOrThrow parser file s =
  case Trifecta.parseString parser (Directed (UTF8.fromString file) 0 0 0 0) s of
    Success a -> return a
    Failure d -> throwError $ ParseError d

readExpr :: FilePath -> String -> ThrowsError LispVal
readExpr = readOrThrow (skipSpaceAndComment *> parseExpr <* skipSpaceAndComment)

readExprList :: FilePath -> String -> ThrowsError [LispVal]
readExprList = readOrThrow $ skipSpaceAndComment *> endBy parseExpr skipSpaceAndComment
