module Main (main) where

import Lexer
import System.Environment

main :: IO ()
main = do
  (expr : _) <- getArgs
  putStrLn (readExpr expr)
