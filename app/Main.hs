module Main (main) where

import Evaluator
import Lexer
import System.Environment
import System.IO
import Types

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = do
  val <- eval env $ readExpr expr
  return $ show val

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result
    then return ()
    else action result >> until_ predicate prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = nullEnv >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    _ -> runOne $ unwords args
