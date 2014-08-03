{-# LANGUAGE OverloadedStrings #-}

module Main where

import WYAS.Parser
import WYAS.Eval hiding (null)
import WYAS.Data (Env, LispVal, liftThrows)

import Control.Monad             (forever, void)
import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Error (runErrorT)

import Data.Text     (Text, pack)
import Data.Foldable (traverse_)
import System.IO     (hFlush, hPrint, stdout, stderr)

import Options.Applicative
import qualified Options.Applicative as Optparse

import Data.Attoparsec.Text

data Opts = Eval    String
          | RunFile String
          | REPL
          deriving (Eq, Show)

opts :: Optparse.Parser Opts
opts =
  (Eval <$> strOption (short 'e' <> metavar "EXPRESSION"))
  <|> (RunFile <$> argument str (metavar "FILENAME"))
  <|> pure REPL

getInput :: String -> IO (Maybe Text)
getInput prompt = do
  putStr prompt
  hFlush stdout
  line <- getLine
  return $ if null line then Nothing else Just $ pack line <> "\n"

handleParseResult :: Result LispVal -> StateT Env IO ()
handleParseResult parseResult =
  case parseResult of
    Fail _ msgs parseErr ->
      traverse_ putErrLn (parseErr : msgs)
    Done _ val -> do
      env <- get
      result <- liftIO $ runEval env (eval val)
      case result of
        Left err  -> putErrLn $ show err
        Right (output, newEnv) -> liftIO (print output) >> put newEnv
    Partial resume ->
      liftIO (getInput "... ") >>= traverse_ (handleParseResult . resume)

  where
    putErrLn msg = liftIO $ putStrLn $ "*** " ++ msg

repl :: IO ()
repl = void $ flip runStateT primitiveBindings $ forever $
  liftIO (getInput ">>> ") >>=
    traverse_ (handleParseResult . parse (skipSpaceAndComment >> parseExpr))

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
    Eval expr -> evalExpr $ pack expr
    RunFile f -> runFile f
    REPL      -> repl
