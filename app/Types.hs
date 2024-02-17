module Types where

-- | LispVal data type for representing Lisp values
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Char Char
  | Bool Bool

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
