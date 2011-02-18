module Crash
     where

main :: IO ()
main = print $ map (*2) [1..3]
