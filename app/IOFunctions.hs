module IOFunctions (ioPrimitives) where

import Data.Functor
import Evaluator (apply)
import Lexer (readExpr, readExprList)
import System.IO
import Types hiding (body, closure, params, vararg)

applyProc :: [LispVal] -> IO LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args
applyProc args = return $ Error $ NumArgs 2 args

makePort :: IOMode -> [LispVal] -> IO LispVal
makePort mode [String filename] = Port <$> openFile filename mode
makePort _ [badArg] = return $ Error $ TypeMismatch "string" badArg
makePort _ badArgList = return $ Error $ NumArgs 1 badArgList

closePort :: [LispVal] -> IO LispVal
closePort [Port port] = hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IO LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = readExpr <$> hGetLine port
readProc [badArg] = return $ Error $ TypeMismatch "port" badArg
readProc badArgList = return $ Error $ NumArgs 1 badArgList

writeProc :: [LispVal] -> IO LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = hPrint port obj >> return (Bool True)
writeProc [_, badArg] = return $ Error $ TypeMismatch "port" badArg
writeProc badArgList = return $ Error $ NumArgs 2 badArgList

readContents :: [LispVal] -> IO LispVal
readContents [String filename] = String <$> readFile filename
readContents [badArg] = return $ Error $ TypeMismatch "string" badArg
readContents badArgList = return $ Error $ NumArgs 1 badArgList

load :: String -> IO [LispVal]
load filename = readFile filename <&> readExprList

readAll :: [LispVal] -> IO LispVal
readAll [String filename] = List <$> load filename
readAll [badArg] = return $ Error $ TypeMismatch "string" badArg
readAll badArgList = return $ Error $ NumArgs 1 badArgList

ioPrimitives :: [(String, [LispVal] -> IO LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-port", closePort),
    ("close-output-port", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]
