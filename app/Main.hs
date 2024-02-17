module Main (main) where

import Evaluator
import Lexer
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
