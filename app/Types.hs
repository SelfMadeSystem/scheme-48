module Types where

import Data.Functor
import Data.IORef
import Data.Maybe
import Text.Parsec (ParseError)
import GHC.IO.Handle

-- | LispError data type for representing Lisp errors
data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String LispVal
  | UnboundVar String String
  | Default String

-- | ThrowsError type for representing Lisp errors
type ThrowsError = Either LispError

-- | LispVal data type for representing Lisp values
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Char Char
  | Bool Bool
  | Port Handle
  | Error LispError
  | PrimitiveFunc ([LispVal] -> LispVal)
  | IOFunc ([LispVal] -> IO LispVal)
  -- | BuiltInFunc is a type for representing special forms and built-in functions
  -- that need access to the environment. e.g. if, cond, define, load, etc.
  | BuiltInFunc (Env -> [LispVal] -> IO LispVal)
  | Func
      { params :: [String],
        vararg :: Maybe String,
        body :: [LispVal],
        closure :: Env
      }

instance Show LispVal where
  show (Atom name) = name
  show (List xs) = "(" ++ unwords (map show xs) ++ ")"
  show (DottedList h t) = "(" ++ unwords (map show h) ++ " . " ++ show t ++ ")"
  show (Number num) = show num
  show (String str) = show str
  show (Char c) =
    "#\\" ++ case c of
      ' ' -> "space"
      '\n' -> "newline"
      '\t' -> "tab"
      '\r' -> "return"
      _ -> [c]
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (Port _) = "<IO port>"
  show (Error err) = showError err
  show (PrimitiveFunc _) = "<primitive>"
  show (IOFunc _) = "<IO primitive>"
  show (BuiltInFunc _) = "<built-in>"
  show (Func {params = args, vararg = varargs, body = _, closure = _}) =
    "(lambda ("
      ++ unwords (map show args)
      ++ ( case varargs of
             Nothing -> ""
             Just arg -> " . " ++ arg
         )
      ++ ") ...)"

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected "
    ++ show expected
    ++ " args; found values "
    ++ unwords (map show found)
showError (TypeMismatch expected found) =
  "Invalid type: expected "
    ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (Default message) = message

instance Show LispError where
  show = showError

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef <&> (isJust . lookup var)

getVar :: Env -> String -> IO LispVal
getVar envRef var = do
  env <- readIORef envRef
  maybe
    (return $ Error $ UnboundVar "Getting an unbound variable" var)
    readIORef
    (lookup var env)

setVar :: Env -> String -> LispVal -> IO LispVal
setVar envRef var value = do
  env <- readIORef envRef
  maybe
    (return $ Error $ UnboundVar "Setting an unbound variable" var)
    (\ref -> writeIORef ref value >> return value)
    (lookup var env)

defineVar :: Env -> String -> LispVal -> IO LispVal
defineVar envRef var value = do
  alreadyDefined <- isBound envRef var
  if alreadyDefined
    then setVar envRef var value $> value
    else do
      valueRef <- newIORef value
      env <- readIORef envRef
      writeIORef envRef ((var, valueRef) : env)
      return value

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
  where
    extendEnv bindings' env = fmap (++ env) (mapM addBinding bindings')
    addBinding (var, value) = do
      ref <- newIORef value
      return (var, ref)
