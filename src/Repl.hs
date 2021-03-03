module Repl where

import Eval
import Data.Text as T

import Control.Monad.Trans
import System.Console.Haskeline

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl

repl :: Repl ()
repl = undefined

process :: String -> IO ()
process = undefined

processToAST :: String -> IO ()
processToAST = undefined