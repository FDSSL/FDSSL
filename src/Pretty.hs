module Pretty where

import Syntax
import Data.List

instance Show Type where
  show TI = "int"
  show TB = "bool"
  show TF = "float"
  show TV2 = "vec2"
  show TV3 = "vec3"
  show TV4 = "vec4"
  show TMat4 = "mat4"
  show _ = error "Some type not implemented yet in show instance for 'Type' in Syntax.hs"

instance Show BOp where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "
  show Mod = " % "

  show And  = " && "
  show Or   = " || "
  show Eq   = " == "
  show Neq  = " != "
  show Gte  = " >= "
  show Gt   = " > "
  show Lte  = " <= "
  show Lt   = " < "

  show BitAnd = " & "
  show BitOr  = " | "
  show BitXor = " ^ "

instance Show Expr where
  show (Mut t n e e') = show t ++ " " ++ n ++ " = " ++ show e ++ ";\n" ++ show e'
  show (Const t n e e') = "const " ++ show t ++ " " ++ n ++ " = " ++ show e ++ ";\n" ++ show e'

  -- internally, the same
  show (Update n e e') = n ++ " = " ++ show e ++ ";\n" ++ show e'
  show (Out n e e') = n ++ " = " ++ show e ++ ";\n" ++ show e'

  show NOp = ""
  show (Branch c t e) = "if (" ++ show c ++ ") {\n" ++ show t ++ "\n} else {\n" ++ show e ++ "\n}\n"
  show (For i (Just n) e t) = "for (int " ++ n ++ " = 0; n < " ++ show i ++ "; "++n++"="++n++"+1) {\n" ++ show e ++ "\n}\n" ++ show t
  show (For i Nothing e t) = "for (int fdssl_cntr = 0; fdssl_cntr < " ++ show i ++ "; fdssl_cntr=fdssl_cntr+1) {\n" ++ show e ++ "\n}\n" ++ show t
  show (SComment s e) = "// " ++ s ++ "\n" ++ show e
  show (BComment s e) = "/* " ++ s ++ " */\n" ++ show e
  show (Seq t t') = show t ++ "\n" ++ show t'
  show (I i) = show i
  show (B b) = if b then "true" else "false"
  show (F f) = show f
  show (D d) = show d
  show (V2 (a,b)) = "vec2(" ++ showParams [a,b] ++ ")"
  show (V3 (a,b,c)) = "vec3(" ++ showParams [a,b,c] ++ ")"
  show (V4 (a,b,c,d)) = "vec4(" ++ showParams [a,b,c,d] ++ ")"
  show (Mat4 m) = "??? mat4 isn't done yet ???"
  show (Ref r) = r
  show (App n ls e) = n ++ "(" ++ showParams ls ++ ");\n" ++ show e
  show (BinOp b e e') = show e ++ show b ++ show e'
  show (AccessN s n) = s ++ "." ++ n
  show (AccessI s i) = s ++ "[" ++ show i ++ "]"
  show _ = error "Undefined Expr present! Invalid program"

instance Show Ret where
  show (Body e "") = "return " ++ show e ++ ";" -- Body should be changed here, perhaps another parameter, a Ref?
  show (Body e r) = show e ++ "\nreturn " ++ r ++ ";"
  show Uniform = "uniform"
  show Attribute = "attribute"
  show Varying = "varying"

instance Show Func where
  show (Func f ls t b@(Body _ _)) = show t ++ " " ++ f ++ "(" ++ concat (intersperse "," (map (\(n,t') -> show t' ++ " " ++ n) ls)) ++ ") {\n" ++ show b ++ "\n}\n"
  show (Func f [] t e) = (++";\n") . concat $ (intersperse " " [show e, show t, f])
  show (Func f _ _ _) = "// External variable " ++ f ++ " has a strange definition\n"

showParams :: [Expr] -> String
showParams ls = concat (intersperse "," (map show ls))



-- | Pretty print a Top level expression (really a statement)
prettyE :: Expr -> String
prettyE = show

-- | Wrap contents in the main func
wrapMain :: String -> String
wrapMain s = "void main() {\n" ++ s ++ "\n}"

-- | Pretty print any general shader
prettyShader :: Shader -> String
prettyShader (Shader _ env1 env2 e) = prettyEnv env1 ++ "\n" ++ prettyEnv env2 ++ "\n" ++ wrapMain (prettyE e)

-- | Pretty print an env w/ a given function and an Env (aka a list of Functions)
--prettyEnv :: (Func -> String) -> Env -> String
--prettyEnv f ls = concat (intersperse "\n" (filter (not . null) (map f ls)))
prettyEnv :: Env -> String
prettyEnv e = concat $ intersperse "\n" $ map show e


-- | Pretty globals, generally
prettyG :: Type -> String -> String
prettyG t n = show t ++ " " ++ n ++ ";"


-- | Combine a vertex shader with a fragment shader, producing a fragment shader with the env from the prior vertex shader
passEnvToShader :: Shader -> Shader -> Shader
passEnvToShader (Shader s _ e _) (Shader s' e' eo expr) = Shader s (e ++ e') eo expr

-- constant preface
preface :: String
preface = "precision highp float;\nprecision highp int;\n"

-- | Pretty print an FDSSL program into a GLSL program
pretty :: Prog -> String
-- 'e' will be brought in as 'uniforms' across all stages
-- 'a' is the attribute env, which goes directly to the vertex shader alone
-- 'v' is the vertex shader, and whatever env is exports is passed to the fragment shader 'f'
pretty (Prog e a v f) = "\n\n===Vertex Shader===\n\n" ++
                        preface ++ prettyEnv (e ++ a) ++
                        prettyShader v ++
                        "\n\n===Fragment Shader===\n\n" ++
                        preface ++ prettyEnv e ++
                        prettyShader (passEnvToShader v f)

prettySafe :: Maybe Prog -> String
prettySafe (Just p) = pretty p
prettySafe Nothing = "Error in Compilation"
