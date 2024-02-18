module Main (main) where

import BuiltInFunctions
import Evaluator
import Functions
import IOFunctions
import Lexer
import System.Environment
import System.IO
import Types

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= flip
      bindVars
      ( map (makeFunc IOFunc) ioPrimitives
          ++ map (makeFunc PrimitiveFunc) primitives
          ++ map (makeFunc BuiltInFunc) builtInsPrimitives
      )
  where
    makeFunc constructor (var, func) = (var, constructor func)

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

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  eval env (List [Atom "load", String (head args)]) >>= hPrint stderr

runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

main :: IO ()
main = do
  args <- getArgs
  if null args then runRepl else runOne args
