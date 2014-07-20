module Main where

import Parser
import Eval
import Data

import Control.Monad (forever, unless)
import System.IO     (hFlush, stdout)

main :: IO ()
main = forever $ do
  putStr "> "
  hFlush stdout
  l <- getLine
  unless (null l) $ putStrLn $ either show show $ runEval nullEnv . eval =<< readExpr l
