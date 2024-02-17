{-# LANGUAGE LambdaCase #-}

module Evaluator (eval) where

import Types

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

-- | Helper function to apply a function to a list of arguments
apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Error $ NotFunction "Unrecognized primitive function args" func) ($ args) $ lookup func primitives

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
cond :: [LispVal] -> LispVal
cond [] = Error $ Default "No true clause found"
cond (List [Atom "else"] : _) = Atom "else"
cond (List (Atom "else" : body) : _) = last $ map eval body
cond (List [predi] : rest) = case eval predi of
  Bool False -> cond rest
  _ -> predi
cond (List (predi : body) : rest) = case eval predi of
  Bool False -> cond rest
  _ -> last $ map eval body
cond badArgList = Error $ BadSpecialForm "Invalid cond clause" $ List badArgList

-- | case is of the form (case expr (clause1) (clause2) ... (clauseN)) where each
-- clause is of the form ((datum1 datum2 ... datumN) body1 body2 ... bodyN). The
-- first clause whose datums match the value of expr (using eqv?) is the one that
-- gets executed. The last clause may be of the form (else body1 body2 ... bodyN)
-- and it will always evaluate to true.
--
-- Note: does not evaluate the datums.
case' :: LispVal -> [LispVal] -> LispVal
case' _ [] = Error $ Default "No true clause found"
case' _ (List [Atom "else"] : _) = Atom "else"
case' _ (List (Atom "else" : body) : _) = last $ map eval body
case' expr (List (List datums : body) : rest) = if any (\datum -> isTruthy $ eqv [expr, datum]) datums then last $ map eval body else case' expr rest
case' _ badArgList = Error $ BadSpecialForm "Invalid case clause" $ List badArgList

-- | and is a special form that evaluates to the first false value or the last
-- value if all values are true
and' :: [LispVal] -> LispVal
and' [] = Bool True
and' [x] = eval x
and' (x : xs) =
  let result = eval x
   in if isTruthy result then and' xs else result

-- | or is a special form that evaluates to the first true value or the last
-- value if all values are false
or' :: [LispVal] -> LispVal
or' [] = Bool False
or' [x] = eval x
or' (x : xs) =
  let result = eval x
   in if isTruthy result then result else or' xs

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Char _) = val
eval val@(Error _) = val
eval (List [Atom "quote", val]) = val
eval (List [Atom "if", predi, conseq, alt]) =
  do
    let result = eval predi
    case result of
      Bool False -> eval alt
      _ -> eval conseq
eval (List (Atom "cond" : clauses)) = cond clauses
eval (List (Atom "case" : expr : clauses)) = case' (eval expr) clauses
eval (List (Atom "and" : args)) = and' args
eval (List (Atom "or" : args)) = or' args
eval (List (Atom func : args)) = apply func $ map eval args
eval badForm = Error $ BadSpecialForm "Unrecognized special form" badForm
