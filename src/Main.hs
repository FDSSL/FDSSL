module Main where

import Syntax
import Pretty
import Examples
import Parser

main :: IO ()
main = do
  evalProg "examples/e0.fdssl"
  evalProg "examples/e1.fdssl"
  evalProg "examples/e2.fdssl"
  evalProg "examples/e3.fdssl"
  evalProg "examples/e4.fdssl"

-- | Evaluates an FDSSL program, and prints the results as GLSL
-- evalProg :: String -> IO ()
-- evalProg fn = do
--   r <- parseFDSSLFile fn
--   case r of
--     (Left l)  -> putStrLn $ show l
--     (Right progList) -> mapM_ putStrLn $ map (pretty . snd) progList

-- | Compiles an FDSSL program into a GLSL Program (vert & frag strings) w/ a name
compileProgram :: (String,Prog) -> IO (String,(String,String))
compileProgram (s,p) = do
  p' <- pretty p
  return (s,p')

-- | Interprets & compiles FDSSL to GLSL, and writes to a file w/ the same name
evalProg :: String -> IO ()
evalProg fn = do
  r <- parseFDSSLFile fn
  case r of
    (Left l)  -> putStrLn $ show l -- failed...
    (Right progList) -> do
      -- compile the individual programs to GLSL shaders
      ls <- mapM compileProgram progList
      -- write out these shaders for each program using
      -- the program name (not the file name)
      mapM (\(s,(v,f)) -> do
        writeFile (s ++ ".vert") v
        writeFile (s ++ ".frag") f
        putStrLn $ "\n* Vertex shader written to " ++ s ++ ".vert"
        putStrLn $ "* Fragment shader written to " ++ s ++ ".frag") ls
      putStrLn $ "* Produced " ++ (show $ length ls) ++ " GLSL program(s)\n"
      return ()
