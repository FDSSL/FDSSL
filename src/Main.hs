module Main where

import Syntax
import Pretty

main :: IO ()
main = do
  _ <- putStrLn $ pretty exampleProg
  putStrLn "Testing"
