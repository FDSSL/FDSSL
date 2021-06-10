{-# LANGUAGE FlexibleInstances #-}


module Pretty where

import Syntax
import Data.List
import Control.Monad

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
  show (Mut t n e) = show t ++ " " ++ n ++ " = " ++ show e ++ ";\n"
  show (Const t n e) = "const " ++ show t ++ " " ++ n ++ " = " ++ show e ++ ";\n"

  -- internally, the same
  show (Update n e) = n ++ " = " ++ show e ++ ";\n"
  show (Out n e) = n ++ " = " ++ show e ++ ";\n"

  show NOp = ""
  show (Branch c t e) = "if (" ++ show c ++ "){\n" ++ prettyBlock t ++ "}\nelse {\n" ++ prettyBlock e ++ "\n}\n"
  show (For i (Just n) e) = "for (int " ++ n ++ " = 0; n < " ++ show i ++ "; "++n++"="++n++"+1) {\n" ++ prettyBlock e ++ "\n}\n"
  show (For i Nothing e) = "for (int fdssl_cntr = 0; fdssl_cntr < " ++ show i ++ "; fdssl_cntr=fdssl_cntr+1) {\n" ++ prettyBlock e ++ "\n}\n"
  show (SComment s) = "// " ++ s ++ "\n"
  show (BComment s) = "/* " ++ s ++ " */\n"
  show (I i) = show i
  show (B b) = if b then "true" else "false"
  show (F f) = show f
  show (D d) = show d
  show (V2 (a,b)) = "vec2" ++ showParams [a,b]
  show (V3 (a,b,c)) = "vec3" ++ showParams [a,b,c]
  show (V4 (a,b,c,d)) = "vec4" ++ showParams [a,b,c,d]
  show (Mat4 m) = "??? mat4 isn't done yet ???"
  show (Ref r) = r
  show (App n ls) = n ++ showParams ls ++ ";\n"
  show (BinOp b e e') = mconcat . intersperse " " $ [show e, show b, show e']
  show (AccessN s n) = mconcat [s, ".", n]
  show (AccessI s i) = mconcat [s, "[", show i, "]"]
  show _ = error "Undefined Expr present! Invalid program"

instance Show OpaqueType where
  show Uniform   = "uniform"
  show Attribute = "attribute"
  show Varying   = "varying"

instance Show Opaque where
  show (Opaque ot t n) = (++";\n") . mconcat $ intersperse " " $ [show ot, show t, n]

instance Show Func where
  show (Func f ls t b) = prettyType f t ++ showParams ls ++ "{\n" ++ prettyBlock b ++ "}\n"

instance {-# OVERLAPS #-} Show (String, Type) where
  show (n, t) = show t ++ " " ++ n

isImm :: Expr -> Bool
isImm (I _) = True
isImm (B _) = True
isImm (F _) = True
isImm (D _) = True
isImm (V2 _) = True
isImm (V3 _) = True
isImm (V4 _) = True
isImm (Mat4 _) = True
isImm (Ref _) = True
isImm (AccessN _ _) = True
isImm (AccessI _ _) = True
isImm (BinOp _ _ _) = True
isImm (App _ _) = True
isImm _ = False


applyIf :: Bool -> (a -> a) -> a -> a
applyIf True  = ($)
applyIf False = flip const

nothingIf :: Bool -> a -> Maybe a
nothingIf True  = const Nothing
nothingIf False = Just

prettyType :: String -> Type -> String
prettyType n t = (show t) ++ " " ++ n


prettyBlock :: Block -> String
prettyBlock (e:[]) = applyIf (isImm e) ret (show e)
prettyBlock (e:es) = show e ++ prettyBlock es
prettyBlock [] =  ""

--prettyBlock (e:es) = return (show e) >>= \s -> prettyBlock es >>= \ss -> return (s ++ ss)

ret :: String -> String
ret s = "return " ++ s ++ ";\n"

prettyFuncs :: ShaderType -> (Maybe ShaderType, Func) -> String
prettyFuncs typ (Just ftyp, f) = if typ == ftyp then show f else ""
prettyFuncs _   (Nothing,   f) = show f

showParams :: Show a => [a] -> String
showParams =  concat . wrapList "(" ")" . intersperse ", " . map show

wrapList :: a -> a -> [a] -> [a]
wrapList l r es = [l] ++ es ++ [r]

-- | Pretty print a Top level expression (really a statement)
prettyE :: Expr -> String
prettyE = show

-- | Wrap contents in the main func
wrapMain :: String -> String
wrapMain s = "void main() {\n" ++ s ++ "\n}"

-- | Pretty print any general shader
prettyShader :: Shader -> String
prettyShader (Shader _ env1 env2 e) = prettyEnv env1 ++ "\n" ++ prettyEnv env2 ++ "\n" ++ wrapMain (concatMap show e)

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
passEnvToShader (Shader _ _ e _) (Shader s' e' eo expr) = Shader s' (e ++ e') eo expr

-- constant preface
preface :: String
preface = "precision highp float;\nprecision highp int;\n"

-- | Pretty print an FDSSL program into a GLSL program
pretty :: Prog -> String
-- 'e' will be brought in as 'uniforms' across all stages
-- 'a' is the attribute env, which goes directly to the vertex shader alone
-- 'v' is the vertex shader, and whatever env is exports is passed to the fragment shader 'f'
pretty (Prog e funcs v f) = "\n\n===Vertex Shader===\n\n" ++
                            preface ++ prettyEnv e ++
                            concatMap (prettyFuncs FragShader) funcs ++
                            prettyShader v ++
                            "\n\n===Fragment Shader===\n\n" ++
                            preface ++ prettyEnv e ++
                            concatMap (prettyFuncs VertShader) funcs ++
                            prettyShader f

prettySafe :: Maybe Prog -> String
prettySafe (Just p) = pretty p
prettySafe Nothing = "Error in Compilation"
