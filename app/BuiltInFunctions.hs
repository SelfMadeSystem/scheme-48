module BuiltInFunctions (builtInsPrimitives) where

import Evaluator
import Functions (eqv)
import IOFunctions (load)
import Types hiding (body, closure, params, vararg)

builtInsPrimitives :: [(String, Env -> [LispVal] -> IO LispVal)]
builtInsPrimitives =
  [ ("apply", applyProc),
    ("load", loadProc),
    ("if", if'),
    ("cond", cond),
    ("case", case'Eval),
    ("define", define),
    ("set!", set),
    ("lambda", lambda),
    ("and", and'),
    ("or", or')
  ]

type IOFunc = Env -> [LispVal] -> IO LispVal

applyProc :: IOFunc
applyProc env (func : args) = do
  func' <- eval env func
  args' <- mapM (eval env) args
  applyProc' func' args'
  where
    applyProc' func' [List args''] = apply env func' args''
    applyProc' func' args'' = apply env func' args''
applyProc _ args = return $ Error $ NumArgs 2 args

loadProc :: IOFunc
loadProc env [String filename] = load filename >>= (fmap last . mapM (eval env))
loadProc _ badArgList = return $ Error $ NumArgs 1 badArgList

-- | if is a special form that evaluates to the second value if the first value
-- is true, otherwise it evaluates to the third value
if' :: IOFunc
if' env [predi, conseq, alt] = do
  result <- eval env predi
  case result of
    Bool False -> eval env alt
    _ -> eval env conseq
if' _ badArgList = return $ Error $ NumArgs 3 badArgList

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

-- | Just calls case' with the first argument evaluated
case'Eval :: Env -> [LispVal] -> IO LispVal
case'Eval env [expr, List clauses] = eval env expr >>= \expr' -> case' env expr' clauses
case'Eval _ badArgList = return $ Error $ NumArgs 2 badArgList

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

-- | define is a special form that binds a value to a variable in the current
-- environment
define :: IOFunc
define env [Atom var, form] = eval env form >>= defineVar env var
define env (List (Atom var : params) : body) = makeNormalFunc env params body >>= defineVar env var
define env (DottedList (Atom var : params) varargs : body) = makeVarArgs varargs env params body >>= defineVar env var
define _ badArgList = return $ Error $ BadSpecialForm "Invalid define form" $ List badArgList

-- | set! is a special form that changes the value of a variable in the current
-- environment
set :: IOFunc
set env [Atom var, form] = eval env form >>= setVar env var
set _ badArgList = return $ Error $ BadSpecialForm "Invalid set! form" $ List badArgList

-- | lambda is a special form that creates a function
lambda :: IOFunc
lambda env (List params : body) = makeNormalFunc env params body
lambda env (DottedList params varargs : body) = makeVarArgs varargs env params body
lambda _ badArgList = return $ Error $ BadSpecialForm "Invalid lambda form" $ List badArgList
