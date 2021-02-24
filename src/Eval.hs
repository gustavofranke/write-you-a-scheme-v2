{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Parser
import LispVal
import qualified Data.Map as Map
import qualified Data.Text as T
import Control.Exception
import Control.Monad.Reader
import Prims

runParseTest :: T.Text -> T.Text
runParseTest input =
  either
    (T.pack . show)
    (T.pack . show)
    $ readExpr input

runASTinEnv :: EnvCtx -> Eval b -> IO b
runASTinEnv code action = runReaderT (unEval action) code

getVar :: LispVal -> Eval LispVal
getVar (Atom atom) = do
    env <- ask
    case Map.lookup atom env of
        Just x -> return x
        Nothing -> throw $ UnboundVar atom

getEven :: [t] -> [t]
getEven [] = []
getEven (x:xs) = x : getOdd xs

getOdd :: [t] -> [t]
getOdd [] = []
getOdd (x:xs) = getEven xs

ensureAtom :: LispVal -> Eval LispVal
ensureAtom n@(Atom _) = return n
ensureAtom n = throw $ TypeMismatch "atom" n

extractVar :: LispVal -> T.Text
extractVar (Atom atom) = atom
