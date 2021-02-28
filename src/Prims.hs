{-# LANGUAGE OverloadedStrings #-}

module Prims where

import Control.Exception
import qualified Data.Text as T
import LispVal
import Control.Monad

type Prim = [(T.Text, LispVal)]

type Unary = LispVal -> Eval LispVal

type Binary = LispVal -> LispVal -> Eval LispVal

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp = undefined

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Fun . IFunc

primEnv :: Prim
primEnv = [
    ("+", mkF $ binopFold (numOp (+)) (Number 0)),
    ("*", mkF $ binopFold (numOp (*)) (Number 1)),
    -- ("++", mkF $ binopFold (strOp (<>)) (String ""))
    ("-", mkF $ binop $ numOp (-))
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