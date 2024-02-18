module Evaluator (eval) where

import Data.Maybe (isNothing)
import Functions
import Types hiding (body, closure, params, vararg)

-- | Helper function to apply a function to a list of arguments
apply :: LispVal -> [LispVal] -> IO LispVal
apply (PrimitiveFunc func) args = return $ func args
apply (Func params vararg body closure) args =
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
apply (Error e) _ = return $ Error e
apply _ _ = return $ Error $ Default "Not implemented yet"

-- | Helper function to check if a value is truthy or falsy
isTruthy :: LispVal -> Bool
isTruthy (Bool False) = False
isTruthy (Error _) = False -- TODO: Handle errors properly
isTruthy _ = True

-- | cond is of the form (cond (clause1) (clause2) ... (clauseN)) where each
-- clause is of the form (predicate body1 body2 ... bodyN). The first clause
-- whose predicate evaluates to true is the one that gets executed. The last
-- clause may be of the form (else body1 body2 ... bodyN) and it will always
-- evaluate to true.
--
-- The standard states that "If all <test>s evaluate to false values, and there
-- is no else clause, then the result of the conditional expression is
-- unspecified", so I'm going to return an error in that case.
cond :: Env -> [LispVal] -> IO LispVal
cond _ [] = return $ Error $ Default "No true clause found"
cond _ (List [Atom "else"] : _) = return $ Atom "else"
cond env (List (Atom "else" : body) : _) = last $ map (eval env) body
cond env (List [predi] : rest) = do
  result <- eval env predi
  case result of
    Bool False -> cond env rest
    _ -> return result
cond env (List (predi : body) : rest) = do
  result <- eval env predi
  case result of
    Bool False -> cond env rest
    _ -> last $ map (eval env) body
cond _ badArgList = return $ Error $ BadSpecialForm "Invalid cond clause" $ List badArgList

-- | case is of the form (case expr (clause1) (clause2) ... (clauseN)) where each
-- clause is of the form ((datum1 datum2 ... datumN) body1 body2 ... bodyN). The
-- first clause whose datums match the value of expr (using eqv?) is the one that
-- gets executed. The last clause may be of the form (else body1 body2 ... bodyN)
-- and it will always evaluate to true.
--
-- Note: does not evaluate the datums.
case' :: Env -> LispVal -> [LispVal] -> IO LispVal
case' _ _ [] = return $ Error $ Default "No true clause found"
case' _ _ (List [Atom "else"] : _) = return $ Atom "else"
case' env _ (List (Atom "else" : body) : _) = last $ map (eval env) body
case' env expr (List (List datums : body) : rest) = if any (\datum -> isTruthy $ eqv [expr, datum]) datums then last $ map (eval env) body else case' env expr rest
case' _ _ badArgList = return $ Error $ BadSpecialForm "Invalid case clause" $ List badArgList

-- | and is a special form that evaluates to the first false value or the last
-- value if all values are true
and' :: Env -> [LispVal] -> IO LispVal
and' _ [] = return $ Bool True
and' env [x] = eval env x
and' env (x : xs) = do
  result <- eval env x
  if isTruthy result then and' env xs else return result

-- | or is a special form that evaluates to the first true value or the last
-- value if all values are false
or' :: Env -> [LispVal] -> IO LispVal
or' _ [] = return $ Bool False
or' env [x] = eval env x
or' env (x : xs) = do
  result <- eval env x
  if isTruthy result then return result else or' env xs

makeFunc :: Maybe String -> Env -> [LispVal] -> [LispVal] -> IO LispVal
makeFunc varargs env params body = return $ Func (map show params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IO LispVal
makeVarArgs = makeFunc . Just . show

eval :: Env -> LispVal -> IO LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Char _) = return val
eval _ val@(Error _) = return val
eval env (Atom ident) = getVar env ident
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", predi, conseq, alt]) = do
  result <- eval env predi
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
eval env (List (Atom "cond" : clauses)) = cond env clauses
eval env (List (Atom "case" : expr : clauses)) = do
  expr' <- eval env expr
  case' env expr' clauses
eval env (List (Atom "and" : args)) = and' env args
eval env (List (Atom "or" : args)) = or' env args
eval env (List (Atom "set!" : [Atom var, form])) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) = makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) = makeVarArgs varargs env params body
eval env (List (Atom func : args)) = do
  argVals <- mapM (eval env) args
  func' <- getVar env func
  apply func' argVals
eval _ badForm = return $ Error $ BadSpecialForm "Unrecognized special form" badForm
