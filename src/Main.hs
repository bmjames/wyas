{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (error)

import WYAS.Parser
import WYAS.Eval hiding (null)
import WYAS.Data (liftThrows)
import WYAS.REPL (replMain)

import Control.Monad.Trans.Class (lift)

import System.IO (hPrint, stderr)

import Options.Applicative hiding (handleParseResult)

import qualified Options.Applicative as Optparse

data Opts = Eval String
          | RunFile FilePath
          | REPL
          deriving (Eq, Show)

parseOpts :: Optparse.Parser Opts
parseOpts =
  (Eval <$> strOption (short 'e' <> metavar "EXPRESSION"))
  <|> (RunFile <$> argument str (metavar "FILENAME"))
  <|> pure REPL

evalExpr :: String -> IO ()
evalExpr expr =
  printEval $ lift (liftThrows $ readExprList expr) >>= evalExprList

runFile :: FilePath -> IO ()
runFile filename =
  printEval $ lift (load filename) >>= evalExprList

printEval :: Show a => EvalIO a -> IO ()
printEval fa = do
  result <- runEval primitiveBindings fa
  either (hPrint stderr) (print . fst) result

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> parseOpts) fullDesc
  case opts of
    Eval expr -> evalExpr expr
    RunFile f -> runFile f
    REPL      -> replMain
