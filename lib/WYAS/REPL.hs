module WYAS.REPL where

import Prelude hiding (error)

import WYAS.Data
import WYAS.Eval hiding (null)
import WYAS.Parser

import Control.Applicative       ((*>), (<$>), (<$))
import Control.Monad             (forever, guard, mzero, void)
import Control.Monad.IO.Class    (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)

import Text.Trifecta
import Text.Trifecta.Delta (Delta(Columns))

import Data.Foldable (traverse_)
import Data.List     (isPrefixOf)
import Data.ByteString.Char8 (ByteString, pack)
import Data.Monoid   (mempty)
import Data.Semigroup.Reducer (snoc)

import System.Console.Haskeline

import qualified Data.Map as Map

parseMultiLine :: Parser a -> InputT EvalIO (Maybe a)
parseMultiLine parseLine =
  runMaybeT $
    go . (`feed` initStep) =<< getInput ">>> "

  where
    go :: Step a -> MaybeT (InputT EvalIO) a
    go (StepFail _ doc) = MaybeT $ Nothing <$ putErrLn (show doc)
    go (StepDone _ a)   = return a
    go (StepCont r _ f) = go . f . snoc r =<< getInput "... "

    initStep = stepParser (release (Columns 0 0) *> parseLine) mempty mempty

evalPrint :: LispVal -> InputT EvalIO ()
evalPrint val = do
  env    <- lift getEnv
  result <- liftIO $ runEval env $ eval val
  case result of
    Left err            -> putErrLn $ show err
    Right (out, newEnv) -> do outputStrLn $ show out
                              lift $ setEnv newEnv

putErrLn :: MonadIO m => String -> InputT m ()
putErrLn msg = outputStrLn $ "*** " ++ msg

getInput :: MonadException f => String -> MaybeT (InputT f) ByteString
getInput prompt = do
  maybeLine <- lift $ getInputLine prompt
  line      <- maybe mzero return maybeLine
  guard $ not $ null line
  return $ pack $ line ++ "\n"

repl :: InputT EvalIO ()
repl = forever $ parseMultiLine parseLine >>= traverse_ evalPrint
  where
    parseLine = skipSpaceAndComment *> parseExpr

replMain :: IO ()
replMain =
  void $ runEval primitiveBindings $ runInputT settings repl

settings = setComplete (addFilenameCompletion completeIdents) defaultSettings

  where
    completeIdents = completeWord Nothing " \n\t()'" matchPrefix

    matchPrefix s = map simpleCompletion . filter (isPrefixOf s) . idents <$> getEnv

    idents env = builtins ++ Map.keys env

    builtins = ["case", "cond", "define", "lambda", "let", "load", "quote"]

    addFilenameCompletion = completeQuotedWord Nothing "\"" listFiles
