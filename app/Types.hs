module Types where

import Text.Parsec (ParseError)

-- | LispError data type for representing Lisp errors
data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
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
  | Error LispError

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
  show (Error err) = showError err

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
