module Crash
     where

import Control.Monad as Mon
import Control.Applicative

main :: IO ()
main = print $ map (*2) [1..3]
