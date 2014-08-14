module WYAS.REPL where

import Prelude hiding (error)

import WYAS.Data
import WYAS.Eval
import WYAS.Parser

import Control.Applicative       ((*>), (<$>))
import Control.Monad             (forever, mzero, void)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import Data.Attoparsec.Text
import Data.Foldable (traverse_)
import Data.List     (isPrefixOf)
import Data.Text     (Text, pack)

import System.Console.Haskeline

import qualified Data.Map as Map

resumeParse :: Result LispVal -> MaybeT (InputT EvalIO) LispVal
resumeParse parseResult = case parseResult of
  Fail _ _ err -> lift $ lift $ error $ Parser err
  Done _ val   -> return val
  Partial f    -> getInput "... " >>= resumeParse . f

evalPrint :: LispVal -> InputT EvalIO ()
evalPrint val = do
  env    <- lift getEnv
  result <- liftIO $ runEval env $ eval val
  case result of
    Left err            -> putErrLn $ show err
    Right (out, newEnv) -> do outputStrLn $ show out
                              lift $ setEnv newEnv

  where
    putErrLn msg = outputStrLn $ "*** " ++ msg

getInput :: MonadException f => String -> MaybeT (InputT f) Text
getInput prompt = do
  line <- lift $ getInputLine prompt
  maybe mzero return $ fmap (pack . (++ "\n")) line

repl :: InputT EvalIO ()
repl = forever $
  runMaybeT (getInput ">>> " >>= resumeParse . parse parseLine) >>=
  traverse_ evalPrint

  where
    parseLine = skipSpaceAndComment *> parseExpr

replMain :: IO ()
replMain = void $ runEval primitiveBindings $
  runInputT settings $ repl

  where
    settings = setComplete (addFilenameCompletion completeIdents) defaultSettings

    completeIdents = completeWord Nothing " \n\t()'" matchPrefix

    matchPrefix s = map simpleCompletion . filter (isPrefixOf s) . idents <$> getEnv

    idents env = builtins ++ Map.keys env

    builtins = ["case", "cond", "define", "lambda", "let", "load", "quote"]

    addFilenameCompletion = completeQuotedWord Nothing "\"" listFiles
