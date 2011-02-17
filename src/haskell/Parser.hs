module Parser
    where

import Language.Haskell.Parser
import Language.Haskell.Pretty

import Control.Applicative

instance Functor ParseResult where
  fmap f (ParseOk x)           = ParseOk $ f x
  fmap f (ParseFailed loc msg) = ParseFailed loc msg

instance Applicative ParseResult where
  pure = ParseOk
  ParseOk f           <*> x = f <$> x
  ParseFailed loc msg <*> _ = ParseFailed loc msg

instance Monad ParseResult where
  return = ParseOk
  ParseOk x           >>= f = f x
  ParseFailed loc msg >>= _ = ParseFailed loc msg

f :: Int -> Maybe Int
f x | x > 5     = Nothing
    | otherwise = Just $ x + 1

main :: IO ()
main = do
  contents <- readFile "./Crash.hs"
  print $ parseModule contents