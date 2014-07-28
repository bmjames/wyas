{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Eval
import Data (LispVal)

import Control.Monad (forever)
import Data.Monoid   ((<>))
import Data.Text     (Text, pack)
import Data.Foldable (traverse_)
import System.IO     (hFlush, stdout)

import Data.Attoparsec.Text

getInput :: String -> IO (Maybe Text)
getInput prompt = do
  putStr prompt
  hFlush stdout
  line <- getLine
  return $ if null line then Nothing else Just $ pack line <> "\n"

handleParseResult :: Result LispVal -> IO ()
handleParseResult parseResult =
  case parseResult of
    Fail _ msgs parseErr ->
      traverse_ putErrLn (parseErr : msgs)
    Done _   val -> 
      let result = runEval nullEnv $ eval val
      in either (putErrLn . show) print result
    Partial resume ->
      getInput "... " >>= traverse_ (handleParseResult . resume)

  where
    putErrLn msg = putStrLn $ "*** " ++ msg

main :: IO ()
main = forever $
  getInput ">>> " >>= traverse_ (handleParseResult . parse parseExpr)
