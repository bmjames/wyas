module Eval where

import Parser

import Control.Monad (forever)

main :: IO ()
main = forever $ putStr "> " >> getLine >>= putStrLn . readExpr
