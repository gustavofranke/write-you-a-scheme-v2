module Repl where

import Control.Monad.Trans
import Data.Text as T
import Eval
import System.Console.Haskeline

type Repl a = InputT IO a

mainLoop :: IO ()
mainLoop = runInputT defaultSettings repl

repl :: Repl ()
repl = do
  minput <- getInputLine "Repl> "
  case minput of
    Nothing -> outputStrLn "Goodbye."
    Just input -> liftIO (process input) >> repl

process :: String -> IO ()
process str = do
    res <- safeExec $ evalText $ T.pack str
    either putStrLn return res

processToAST :: String -> IO ()
processToAST str = print $ runParseTest $ T.pack str
