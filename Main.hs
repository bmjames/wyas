{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Eval
import Data (LispVal)

import Control.Monad (forever)
import Data.Monoid   ((<>))
import Data.Text     (Text, pack)
import System.IO     (hFlush, stdout)

import Data.Attoparsec.Text

getInput :: String -> IO Text
getInput prompt = do
  putStr prompt
  hFlush stdout
  line <- getLine
  return $ pack line <> "\n"

handleParseResult :: Result LispVal -> IO ()
handleParseResult parseResult =
  case parseResult of
    Fail _ _ parseErr -> putStrLn $ "*** " ++ parseErr
    Done _   val      -> 
      let output = either show show result
          result = runEval nullEnv $ eval val
      in putStrLn output
    Partial resume    -> getInput "... " >>= handleParseResult . resume

main :: IO ()
main = forever $
  getInput ">>> " >>= handleParseResult . parse parseExpr

