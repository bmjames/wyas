{-# LANGUAGE OverloadedStrings #-}

module Main where

import WYAS.Parser
import WYAS.Eval (eval, runEval, primitiveBindings)
import WYAS.Data (Env, LispVal)

import Control.Monad             (forever, void)
import Control.Monad.Trans.State (StateT, runStateT, get, put)
import Control.Monad.IO.Class    (liftIO)

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

main :: IO ()
main = void $ flip runStateT primitiveBindings $ forever $
  liftIO (getInput ">>> ") >>=
    traverse_ (handleParseResult . parse (skipSpaceAndComment >> parseExpr))
