--{-# LANGUAGE GADTs #-}
module Main where

import Syntax
import Pretty
import Examples
import Parser

main :: IO ()
main = putStrLn $ prettySafe prog1

evalProg :: String -> IO ()
evalProg fn = do
  r <- parseFDSSLFile fn
  case r of
    (Left l)  -> putStrLn $ show l
    (Right progList) -> mapM_ putStrLn $ map (pretty . snd) progList

-- needs fixing or removal
-- main2 :: IO ()
-- main2 = putStrLn $ prettySafe prog2
