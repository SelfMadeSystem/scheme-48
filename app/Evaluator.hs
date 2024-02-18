module Evaluator (eval, apply) where

import Data.Functor
import Data.Maybe (isNothing)
import Types hiding (body, closure, params, vararg)

applyFunc :: [String] -> Maybe String -> [LispVal] -> Env -> [LispVal] -> IO LispVal
applyFunc params vararg body closure args = do
  if num params /= num args && isNothing vararg || num args < num params
    then return $ Error $ NumArgs (num params) args
    else bindVars closure (zip params args) >>= bindVarArgs vararg >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs arg env = case arg of
      Just argName -> bindVars env [(argName, List remainingArgs)]
      Nothing -> return env

-- | Helper function to apply a function to a list of arguments
apply :: Env -> LispVal -> [LispVal] -> IO LispVal
apply env (PrimitiveFunc func) args = mapM (eval env) args <&> func
apply env (IOFunc func) args = mapM (eval env) args >>= func
apply env (BuiltInFunc func) args = func env args
apply env (Func params vararg body closure) args =
  mapM (eval env) args
    >>= applyFunc params vararg body closure
apply _ (Error e) _ = return $ Error e
apply _ badForm _ = return $ Error $ BadSpecialForm "Unrecognized special form" badForm

eval :: Env -> LispVal -> IO LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Char _) = return val
eval _ val@(Error _) = return val
eval _ val@(PrimitiveFunc _) = return val
eval _ val@(IOFunc _) = return val
eval _ val@(BuiltInFunc _) = return val
eval _ val@(Func {}) = return val
eval env (Atom ident) = getVar env ident
eval _ (List [Atom "quote", val]) = return val
eval env (List (Atom func : args)) = do
  func' <- getVar env func
  apply env func' args
eval _ badForm = return $ Error $ BadSpecialForm "Unrecognized special form" badForm
