module WYAS.REPL where

import Prelude hiding (error)

import WYAS.Data
import WYAS.Eval hiding (null)
import WYAS.Parser

import Control.Applicative       ((*>), (<$>))
import Control.Monad             (forever, guard, mzero, void)
import Control.Monad.IO.Class    (liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

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
  runMaybeT $ go . flip feed (stepParser (release d *> parseLine) mempty mempty) =<< getInput ">>> "

  where
    go (StepFail r doc) = lift $ lift $ error $ ParseError doc
    go (StepDone _ a)   = return a
    go (StepCont r _ f) = go . f . snoc r =<< getInput "... "

    d = Columns 0 0

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

  where
    settings = setComplete (addFilenameCompletion completeIdents) defaultSettings

    completeIdents = completeWord Nothing " \n\t()'" matchPrefix

    matchPrefix s = map simpleCompletion . filter (isPrefixOf s) . idents <$> getEnv

    idents env = builtins ++ Map.keys env

    builtins = ["case", "cond", "define", "lambda", "let", "load", "quote"]

    addFilenameCompletion = completeQuotedWord Nothing "\"" listFiles
