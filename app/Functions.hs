{-# LANGUAGE LambdaCase #-}

module Functions (primitives, eqv) where

import Types hiding (body, closure, params, vararg)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (Error e) = Left e
unpackNum notNum = Left $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool (Error e) = Left e
unpackBool notBool = Left $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Error e) = Left e
unpackStr notString = Left $ TypeMismatch "string" notString

-- | Helper function to make a numeric operation that takes two or more
-- arguments
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop _ [] = Error $ NumArgs 2 []
numericBinop _ singleVal@[_] = Error $ NumArgs 2 singleVal
numericBinop op params = case mapM unpackNum params of
  Left err -> Error err
  Right numbers -> Number $ foldl1 op numbers

-- | Helper function to compose the numBoolBinop, boolBoolBinop and strBoolBinop
-- functions
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> LispVal
boolBinop unpacker op args =
  if length args /= 2
    then Error $ NumArgs 2 args
    else do
      let leftEither = unpacker $ head args
      let rightEither = unpacker $ args !! 1
      case leftEither of
        Left err -> Error err
        Right left -> do
          case rightEither of
            Left err -> Error err
            Right right -> Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> LispVal
numBoolBinop = boolBinop unpackNum

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> LispVal
boolBoolBinop = boolBinop unpackBool

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> LispVal
strBoolBinop = boolBinop unpackStr

-- | Helper function to check if a value is of a certain type
isType :: (LispVal -> Bool) -> [LispVal] -> LispVal
isType _ [] = Error $ NumArgs 1 []
isType f [x] = Bool $ f x
isType _ args = Error $ NumArgs 1 args

isBool :: [LispVal] -> LispVal
isBool =
  isType
    ( \case
        Bool _ -> True
        _ -> False
    )

isSymbol :: [LispVal] -> LispVal
isSymbol =
  isType
    ( \case
        Atom _ -> True
        _ -> False
    )

isString :: [LispVal] -> LispVal
isString =
  isType
    ( \case
        String _ -> True
        _ -> False
    )

isNumber :: [LispVal] -> LispVal
isNumber =
  isType
    ( \case
        Number _ -> True
        _ -> False
    )

isChar :: [LispVal] -> LispVal
isChar =
  isType
    ( \case
        Char _ -> True
        _ -> False
    )

isList :: [LispVal] -> LispVal
isList =
  isType
    ( \case
        List _ -> True
        _ -> False
    )

isDottedList :: [LispVal] -> LispVal
isDottedList =
  isType
    ( \case
        DottedList _ _ -> True
        _ -> False
    )

-- | car is equivalent to head in Haskell
car :: [LispVal] -> LispVal
car [List (x : _)] = x
car [DottedList (x : _) _] = x
car [badArg] = Error $ TypeMismatch "pair" badArg
car badArgList = Error $ NumArgs 1 badArgList

-- | cdr is equivalent to tail in Haskell
cdr :: [LispVal] -> LispVal
cdr [List (_ : xs)] = List xs
cdr [DottedList [_] x] = x
cdr [DottedList (_ : xs) x] = DottedList xs x
cdr [badArg] = Error $ TypeMismatch "pair" badArg
cdr badArgList = Error $ NumArgs 1 badArgList

-- | cons is equivalent to (:) in Haskell
cons :: [LispVal] -> LispVal
cons [x, List xs] = List $ x : xs
cons [x, DottedList xs xlast] = DottedList (x : xs) xlast
cons [x1, x2] = DottedList [x1] x2
cons badArgList = Error $ NumArgs 2 badArgList

-- | eqv? is equivalent to (==) in Haskell. We're using it for the eq?, eqv? and
-- equal? functions. The tutorial tells me to use make equal? check for weak
-- equality, but I don't like weak typing, so I said no :).
eqv :: [LispVal] -> LispVal
eqv [Bool arg1, Bool arg2] = Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = Bool $ arg1 == arg2
eqv [String arg1, String arg2] = Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = Bool $ arg1 == arg2
eqv [Char arg1, Char arg2] = Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] = Bool $ (length arg1 == length arg2) && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Bool val -> val
      _ -> False
eqv [_, _] = Bool False
eqv badArgList = Error $ NumArgs 2 badArgList

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop div),
    ("mod", numericBinop mod),
    ("quotient", numericBinop quot),
    ("remainder", numericBinop rem),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=)),
    ("symbol?", isSymbol),
    ("string?", isString),
    ("number?", isNumber),
    ("char?", isChar),
    ("bool?", isBool),
    ("list?", isList),
    ("dotted-list?", isDottedList),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eq?", eqv),
    ("eqv?", eqv),
    ("equal?", eqv)
  ]
