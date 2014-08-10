{-# LANGUAGE OverloadedStrings #-}

module Main where

import Prelude hiding (error)

import WYAS.Parser
import WYAS.Eval hiding (null)
import WYAS.Data (LispVal, LispError(Parser), liftThrows)

import Control.Monad             (forever, mzero, void, (<=<))
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)

import Data.Text     (Text, pack)
import Data.Foldable (traverse_)
import Data.List     (isPrefixOf)

import System.Console.Haskeline
import System.IO (hPrint, stderr)

import Options.Applicative hiding (handleParseResult)

import qualified Data.Map            as Map
import qualified Options.Applicative as Optparse

import Data.Attoparsec.Text

data Opts = Eval    Text
          | RunFile FilePath
          | REPL
          deriving (Eq, Show)

opts :: Optparse.Parser Opts
opts =
  (Eval . pack <$> strOption (short 'e' <> metavar "EXPRESSION"))
  <|> (RunFile <$> argument str (metavar "FILENAME"))
  <|> pure REPL

resumeParse :: Result LispVal -> InputT (EvalT (MaybeT IO)) LispVal
resumeParse parseResult = case parseResult of
  Fail _ _ err -> lift $ error $ Parser err
  Done _ val   -> return val
  Partial f    -> getInput "... " >>=
                  lift . maybe mzero return >>=
                  resumeParse . f

evalPrint :: LispVal -> InputT (EvalT (MaybeT IO)) ()
evalPrint val = do
  env    <- lift getEnv
  result <- liftIO $ runEval env $ eval val
  case result of
    Left err            -> putErrLn $ show err
    Right (out, newEnv) -> do outputStrLn $ show out
                              lift $ setEnv newEnv

  where
    putErrLn msg = outputStrLn $ "*** " ++ msg

getInput :: MonadException f => String -> InputT f (Maybe Text)
getInput = fmap (fmap $ pack . (++ "\n")) . getInputLine

repl :: IO ()
repl = void $ runMaybeT $ runEval primitiveBindings$
  runInputT settings $ forever $
    getInput ">>> " >>=
    traverse_ (evalPrint <=< resumeParse . parse parseLine)

  where
    parseLine = skipSpaceAndComment *> parseExpr

    settings = setComplete (addFilenameCompletion completeIdents) defaultSettings

    completeIdents = completeWord Nothing " \n\t()'" matchPrefix

    matchPrefix s = map simpleCompletion . filter (isPrefixOf s) . idents <$> getEnv

    idents env = builtins ++ Map.keys env

    builtins = ["case", "cond", "define", "lambda", "let", "load", "quote"]

    addFilenameCompletion = completeQuotedWord Nothing "\"" listFiles

evalExpr :: Text -> IO ()
evalExpr expr =
  printEval $ lift (liftThrows $ readExprList expr) >>= evalExprList

runFile :: String -> IO ()
runFile filename =
  printEval $ lift (load filename) >>= evalExprList

printEval :: Show a => EvalIO a -> IO ()
printEval fa = do
  result <- runEval primitiveBindings fa
  either (hPrint stderr) (print . fst) result

main :: IO ()
main = do
  opts <- execParser $ info (helper <*> opts) fullDesc
  case opts of
    Eval expr -> evalExpr expr
    RunFile f -> runFile f
    REPL      -> repl
