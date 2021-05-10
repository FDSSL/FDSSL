module Pretty where

import Syntax
import Data.List

-- | Pretty print a Top level expression (really a statement)
prettyE :: TExpr -> String
prettyE = show

-- | Wrap contents in the main func
wrapMain :: String -> String
wrapMain s = "void main() {\n" ++ s ++ "\n}"

-- | Pretty print any general shader
prettyShader :: Env -> TExpr -> String
prettyShader env e = prettyEnv prettyVary env ++ "\n" ++ wrapMain (prettyE e)

-- | Pretty print a vertex shader into a GLSL equivalent
prettyVert :: VertShader -> String
prettyVert (VertShader env e) = prettyShader env e

-- | Pretty print a fragment shader into a GLSL equivalent
prettyFrag :: FragShader -> String
prettyFrag (FragShader env e) = prettyShader env e

-- | Pretty print an env w/ a given function and an Env (aka a list of Functions)
prettyEnv :: (Func -> String) -> Env -> String
prettyEnv f ls = concat (intersperse "\n" (filter (not . null) (map f ls)))

-- | Pretty globals, generally
prettyG :: Type -> String -> String
prettyG t n = show t ++ " " ++ n ++ ";"

-- | Pretty print attribute vars, going into the vertex shader
prettyAttr :: Func -> String
prettyAttr (Func n _ t e) = "attribute " ++ prettyG t n

-- | Pretty print uniforms
prettyUni :: Func -> String
-- just a uniform
prettyUni (Func n [] t e) = "uniform " ++ prettyG t n
prettyUni _ = ""

-- | Pretty print universal functions (available across all shaders)
prettyFunc :: Func -> String
prettyFunc (Func n [] t e) = ""
prettyFunc f = show f

-- | Pretty print 'varying' or in/out variables
prettyVary :: Func -> String
prettyVary (Func n _ t e) = "varying " ++ prettyG t n

-- | Combine a vertex shader with a fragment shader, producing a fragment shader with the env from the prior vertex shader
passEnvToFrag :: VertShader -> FragShader -> FragShader
passEnvToFrag (VertShader e _) (FragShader e' expr) = FragShader (e ++ e') expr

-- constant preface
preface :: String
preface = "precision highp float;\nprecision highp int;\n"

-- | Pretty print an FDSSL program into a GLSL program
pretty :: Prog -> String
-- 'e' will be brought in as 'uniforms' across all stages
-- 'a' is the attribute env, which goes directly to the vertex shader alone
-- 'v' is the vertex shader, and whatever env is exports is passed to the fragment shader 'f'
pretty (Prog e a v f) = "\n\n===Vertex Shader===\n\n" ++ preface ++ unis ++ attrs ++ funcs ++ prettyVert v ++ "\n\n===Fragment Shader===\n\n" ++ preface ++ unis ++ funcs ++ prettyFrag (passEnvToFrag v f)
                        where
                          unis  = prettyEnv prettyUni e ++ "\n"
                          attrs = prettyEnv prettyAttr a ++ "\n"
                          funcs = prettyEnv prettyFunc e
