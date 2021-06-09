--{-# LANGUAGE GADTs #-}
module Main where

import Syntax
import Pretty
import Examples
import Parser

main :: IO ()
main = putStrLn $ prettySafe prog1

-- | Evaluates an FDSSL program, and prints the results as GLSL
evalProg :: String -> IO ()
evalProg fn = do
  r <- parseFDSSLFile fn
  case r of
    (Left l)  -> putStrLn $ show l
    (Right progList) -> mapM_ putStrLn $ map (pretty . snd) progList

-- | Crude representation for now
-- TODO Change this for a proper representation style later...
type GLSLProg = String

-- | Compiles an FDSSL program into a GLSL Program
--compileProgram :: Prog -> GLSLProg
--compileProgram p =

-- | Interprets & compiles FDSSL to GLSL, and writes to a file w/ the same name
{-
compile :: String -> IO ()
compile fn = do
  r <- parseFDSSLFile fn
  case r of
    (Left l)  -> putStrLn $ show l -- failed...
    (Right progList) -> mapM_ compileProgram progList
-}

      --mapM_ putStrLn $ map (pretty . snd) progList

-- needs fixing or removal
-- main2 :: IO ()
-- main2 = putStrLn $ prettySafe prog2
