{-# LANGUAGE OverloadedStrings #-}

module Prims where

import Control.Exception
import qualified Data.Text as T
import LispVal
import Control.Monad
import Control.Monad.Except
import System.IO
import qualified Data.Text.IO as TIO

type Prim = [(T.Text, LispVal)]

type Unary = LispVal -> Eval LispVal

type Binary = LispVal -> LispVal -> Eval LispVal

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Fun . IFunc

primEnv :: Prim
primEnv = [
    ("+", mkF $ binopFold (numOp (+)) (Number 0)),
    ("*", mkF $ binopFold (numOp (*)) (Number 1)),
    ("++", mkF $ binopFold (strOp (<>)) (String "")),
    ("-", mkF $ binop $ numOp (-)),
    ("<", mkF $ binop $ numCmp (<)),
    ("<=", mkF $ binop $ numCmp (<=)),
    (">", mkF $ binop $ numCmp (>)),
    (">=", mkF $ binop $ numCmp (>=)),
    ("==", mkF $ binop $ numCmp (==)),
    ("even?", mkF $ unop $ numBool even),
    ("odd?", mkF $ unop $ numBool odd),
    ("pos?", mkF $ unop $ numBool (< 0)),
    ("neg?", mkF $ unop $ numBool (> 0)),
    ("eq?", mkF $ binop eqCmd),
    ("bl-eq?", mkF $ binop $ eqOp (==)),
    ("and?", mkF $ binopFold (eqOp (&&)) (Bool True)),
    ("or", mkF $ binopFold (eqOp (||)) (Bool False)),
    ("cons", mkF Prims.cons),
    ("cdr", mkF Prims.cdr),
    ("car", mkF Prims.car),
    ("file?", mkF $ unop fileExists),
    ("slurp", mkF $ unop slurp)
  ]

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x] = op x
unop _ args = throw $ NumArgs 1 args

binop :: Binary -> [LispVal] -> Eval LispVal
binop op [x,y] = op  x y
binop _ args = throw $ NumArgs 2 args

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold op farg args = case args of
  [a,b] -> op a b
  (a:as) -> foldM op farg args
  [] -> throw $ NumArgs 2 args

strOp :: (T.Text -> T.Text -> T.Text) -> LispVal -> LispVal -> Eval LispVal
strOp op (String x) (String y) = return $ String $ op x y
strOp op x          (String y) = undefined
strOp op (String x) y = undefined
strOp op x          y = undefined 

fileExists :: LispVal -> Eval LispVal
fileExists (Atom atom) = undefined
fileExists (String txt) = undefined
fileExists val = throw $ TypeMismatch "expects str, got: " val

slurp :: LispVal -> Eval LispVal
slurp (String txt) = liftIO $ wFileSlurp txt
slurp val = throw $ TypeMismatch "expects str, got: " val

wFileSlurp :: T.Text -> IO LispVal 
wFileSlurp fileName = withFile (T.unpack fileName) ReadMode go
  where
    go = readTextFile fileName

readTextFile :: T.Text -> Handle -> IO LispVal 
readTextFile fileName handle = do
  exists <- hIsEOF handle
  if exists
  then (TIO.hGetContents handle) >>= (return . String)
  else throw $ IOError $ T.concat [" file does not exist: ", fileName]

cons :: [LispVal]  -> Eval LispVal
cons [x, y@(List yList)] = return $ List $ x:y:yList
cons [c] = return $ List [c]
cons [] = return $ List []
cons _ = throw $ ExpectedList "cons, in second argument"

car :: [LispVal] -> Eval LispVal
car [List []] = return Nil
car [List (x:_)] = undefined
car [] = undefined
car x = undefined

cdr :: [LispVal] -> Eval LispVal
cdr [List (x:xs)] = undefined
cdr [List []] = undefined 
cdr [] = undefined 
cdr x = undefined

numBool :: (Integer -> Bool) -> LispVal -> Eval LispVal 
numBool op (Number x) = undefined 
numBool op x = undefined

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp = undefined

eqOp :: (Bool -> Bool -> Bool) -> LispVal -> LispVal -> Eval LispVal
eqOp op (Bool x) (Bool y) = return $ Bool $ op x y
eqOp op x (Bool y) = undefined
eqOp op (Bool x) y = undefined
eqOp op x y = undefined

numCmp :: (Integer -> Integer -> Bool) -> LispVal -> LispVal -> Eval LispVal
numCmp op (Number x) (Number y) = undefined
numCmp op x (Number y) = undefined
numCmp op (Number x) y = undefined
numCmp op x y = undefined

eqCmd :: LispVal -> LispVal -> Eval LispVal
eqCmd (Atom x) (Atom y)= undefined
eqCmd (Number x) (Number y) = undefined
eqCmd (String x) (String y) = undefined
eqCmd (Bool x) (Bool y) = undefined
eqCmd Nil Nil = undefined
eqCmd _ _ = undefined
