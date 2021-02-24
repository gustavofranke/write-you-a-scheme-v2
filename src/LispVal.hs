{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module LispVal where

import Control.Exception
import Control.Monad.Except
import Control.Monad.Reader
import Data.Data
import qualified Data.Map as Map
import qualified Data.Text as T

data LispVal
  = Atom T.Text
  | List [LispVal]
  | Number Integer
  | String T.Text
  | Fun IFunc
  | Lambda IFunc EnvCtx
  | Nil
  | Bool Bool
  deriving (Typeable)

data IFunc = IFunc {fn :: [LispVal] -> Eval LispVal}

instance Show LispVal where
  show = T.unpack . showVal

showVal :: LispVal -> T.Text
showVal val =
  case val of
    (Atom atom) -> atom
    (String str) -> T.concat ["\"", str, "\""]
    (Number num) -> T.pack $ show num
    (Bool True) -> "#t"
    (Bool False) -> "#f"
    Nil -> "Nil"
    (List contents) -> T.concat ["(", T.unwords $ showVal <$> contents, ")"]
    (Fun _) -> "(internal function)"
    (Lambda _ _) -> "(lambda function)"

--------------

type EnvCtx = Map.Map T.Text LispVal

newtype Eval a = Eval {unEval :: ReaderT EnvCtx IO a}
  deriving
    ( Monad,
      Functor,
      Applicative,
      MonadReader EnvCtx,
      MonadIO
    )

data LispException
  = NumArgs Integer [LispVal]
  | TypeMismatch T.Text LispVal
  | BadSpecialForm T.Text
  | NotFunction LispVal
  | UnboundVar T.Text
  | PError String

instance Exception LispException

instance Show LispException where
  show = undefined
