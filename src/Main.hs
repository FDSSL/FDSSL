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
main = putStrLn $ pretty e5
