{-# LANGUAGE OverloadedStrings #-}

module Prims where

import Control.Exception
import qualified Data.Text as T
import LispVal

type Prim = [(T.Text, LispVal)]

type Unary = LispVal -> Eval LispVal

type Binary = LispVal -> LispVal -> Eval LispVal

numOp :: (Integer -> Integer -> Integer) -> LispVal -> LispVal -> Eval LispVal
numOp = undefined

binopFold :: Binary -> LispVal -> [LispVal] -> Eval LispVal
binopFold = undefined

mkF :: ([LispVal] -> Eval LispVal) -> LispVal
mkF = Fun . IFunc

primEnv :: Prim
primEnv = []

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x] = op x
unop _ args = throw $ NumArgs 1 args
