module Main
    where

import System.Environment

import Language.Haskell.Parser
import Language.Haskell.Syntax

import Serialize

stripResult :: ParseResult HsModule -> HsModule
stripResult (ParseOk m)       = m
stripResult (ParseFailed _ _) = undefined

main :: IO ()
main = do
  args     <- getArgs
  contents <- readFile (head args)
  putStrLn $ (serialize . stripResult . parseModule) contents