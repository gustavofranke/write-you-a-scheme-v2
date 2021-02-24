module Prims where

import Control.Exception
import qualified Data.Text as T
import LispVal

type Prim = [(T.Text, LispVal)]

type Unary = LispVal -> Eval LispVal

primEnv :: Prim
primEnv = []

unop :: Unary -> [LispVal] -> Eval LispVal
unop op [x] = op x
unop _ args = throw $ NumArgs 1 args
