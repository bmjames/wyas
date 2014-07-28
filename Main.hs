{-# LANGUAGE OverloadedStrings #-}

module Main where

import Parser
import Eval
import Data (LispVal)

import Control.Monad          (forever, void)
import Control.Monad.State    (StateT, runStateT, get, put)
import Control.Monad.IO.Class (liftIO)

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
    Done _   val -> do
      env <- get
      let (result, newEnv) = runEval env (eval val)
      put newEnv
      either (putErrLn . show) (liftIO . print) result
    Partial resume ->
      liftIO (getInput "... ") >>= traverse_ (handleParseResult . resume)

  where
    putErrLn msg = liftIO $ putStrLn $ "*** " ++ msg

main :: IO ()
main = void $ flip runStateT nullEnv $ forever $
  liftIO (getInput ">>> ") >>= traverse_ (handleParseResult . parse parseExpr)
