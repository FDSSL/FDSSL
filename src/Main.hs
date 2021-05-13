--{-# LANGUAGE GADTs #-}
module Main where

import Syntax
import Pretty
import Examples

------------
import Data.Matrix
import Data.Vector
import Data.Typeable
import Numeric.Natural
------------


main :: IO ()
main = putStrLn $ prettySafe prog1

main2 :: IO ()
main2 = putStrLn $ prettySafe prog2
