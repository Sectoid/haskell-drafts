{-# LANGUAGE ExistentialQuantification #-}

module Main
    where

import System.Environment (getArgs)

import Language.Haskell.Parser
import Language.Haskell.Syntax

import Serialize

data ShowBox = forall s. Show s => SB s


instance Show ShowBox where
    show (SB s) = show s

a, b :: Int
a = 1
b = 2
 
heteroList :: [ShowBox]
heteroList = [SB (), SB 5, SB True]

stripResult :: ParseResult HsModule -> HsModule
stripResult (ParseOk m)       = m
stripResult (ParseFailed _ _) = undefined

main :: IO ()
main = do
  args     <- getArgs
  contents <- readFile (head args)
  putStrLn $ (serialize . stripResult . parseModule) contents