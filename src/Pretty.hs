{-# LANGUAGE FlexibleInstances #-}

module Pretty where

import Control.Monad.State

import Syntax
import Data.List
import Control.Monad

-- | Simple show instances for types
instance Show Type where
  show TI = "int"
  show TB = "bool"
  show TF = "float"
  show TV2 = "vec2"
  show TV3 = "vec3"
  show TV4 = "vec4"
  show TMat4 = "mat4"
  show _ = error "Some type not implemented yet in show instance for 'Type' in Syntax.hs"

-- | Simple show instances for BinOps
instance Show BOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show Mod = "%"

  show And  = "&&"
  show Or   = "||"
  show Eq   = "=="
  show Neq  = "!="
  show Gte  = ">="
  show Gt   = ">"
  show Lte  = "<="
  show Lt   = "<"

  show BitAnd = "&"
  show BitOr  = "|"
  show BitXor = "^"

-- TODO delete this...
-- instance Show Expr where
--   show (Mut t n e) = show t ++ " " ++ n ++ " = " ++ show e ++ ";\n"
--   show (Const t n e) = "const " ++ show t ++ " " ++ n ++ " = " ++ show e ++ ";\n"
--
--   -- internally, the same
--   show (Update n e) = n ++ " = " ++ show e ++ ";\n"
--   show (Out n e) = n ++ " = " ++ show e ++ ";\n"
--
--   show NOp = ""
--   show (Branch c t e) = "if (" ++ show c ++ "){\n" ++ show t ++ "}\nelse {\n" ++ show e ++ "\n}\n"
--   show (For i (Just n) e) = "for (int " ++ n ++ " = 0; n < " ++ show i ++ "; "++n++"="++n++"+1) {\n" ++ show e ++ "\n}\n"
--   show (For i Nothing e) = "for (int fdssl_cntr = 0; fdssl_cntr < " ++ show i ++ "; fdssl_cntr=fdssl_cntr+1) {\n" ++ show e ++ "\n}\n"
--   show (SComment s) = "// " ++ s ++ "\n"
--   show (BComment s) = "/* " ++ s ++ " */\n"
--   show (I i) = show i
--   show (B b) = if b then "true" else "false"
--   show (F f) = show f
--   show (D d) = show d
--   show (V2 (a,b)) = "vec2" ++ showParams [a,b]
--   show (V3 (a,b,c)) = "vec3" ++ showParams [a,b,c]
--   show (V4 (a,b,c,d)) = "vec4" ++ showParams [a,b,c,d]
--   show (Mat4 m) = "??? mat4 isn't done yet ???"
--   show (Ref r) = r
--   show (App n ls) = n ++  show ls ++ ";\n"
--   show (BinOp b e e') = mconcat . intersperse " " $ [show e, show b, show e']
--   show (AccessN s n) = mconcat [s, ".", n]
--   show (AccessI s i) = mconcat [s, "[", show i, "]"]
--   show _ = error "Undefined Expr present! Invalid program"

instance Show OpaqueType where
  show Uniform   = "uniform"
  show Attribute = "attribute"
  show Varying   = "varying"

instance Show Opaque where
  show (Opaque ot t n) = (++";\n") . mconcat $ intersperse " " $ [show ot, show t, n]
--
-- instance Show Func where
--   show (Func f ls t b) = case prettyBlock b of
--                            Just body -> prettyType f t ++ showParams ls ++ "{\n" ++ body ++ "}\n"
--                            Nothing -> "// Function " ++ show f ++ " failed to parse"

-- instance {-# OVERLAPS #-} Show (String, Type) where
--   show (n, t) = show t ++ " " ++ n
--
-- isImm :: Expr -> Bool
-- isImm (I _) = True
-- isImm (B _) = True
-- isImm (F _) = True
-- isImm (D _) = True
-- isImm (V2 _) = True
-- isImm (V3 _) = True
-- isImm (V4 _) = True
-- isImm (Mat4 _) = True
-- isImm (Ref _) = True
-- isImm (AccessN _ _) = True
-- isImm (AccessI _ _) = True
-- isImm (BinOp _ _ _) = True
-- isImm _ = False


-- applyIf :: Bool -> (a -> a) -> a -> a
-- applyIf True  = ($)
-- applyIf False = flip const
--
-- nothingIf :: Bool -> a -> Maybe a
-- nothingIf True  = const Nothing
-- nothingIf False = Just
--
-- prettyType :: String -> Type -> String
-- prettyType n t = (show t) ++ " " ++ n
--
-- prettyBlock :: Block -> Maybe String
-- prettyBlock (e:[]) = Just $ applyIf (isImm e) ret (show e)
-- prettyBlock (e:es) = liftM2 (++) (nothingIf (isImm e) (show e)) (prettyBlock es)
-- prettyBlock [] = Just ""

--prettyBlock (e:es) = return (show e) >>= \s -> prettyBlock es >>= \ss -> return (s ++ ss)

-- ret :: String -> String
-- ret s = "return " ++ s ++ ";\n"
--
-- prettyFuncs :: ShaderType -> (Maybe ShaderType, Func) -> String
-- prettyFuncs typ (Just ftyp, f) = if typ == ftyp then show f else ""
-- prettyFuncs _   (Nothing,   f) = show f
--
-- showParams :: Show a => [a] -> String
-- showParams =  concat . wrapList "(" ")" . intersperse ", " . map show
--

-- -- | Pretty print a Top level expression (really a statement)
-- prettyE :: Expr -> String
-- prettyE = show
--
-- -- | Wrap contents in the main func
-- wrapMain :: String -> String
-- wrapMain s = "void main() {\n" ++ s ++ "\n}"
--
-- -- | Pretty print any general shader
-- prettyShader :: Shader -> String
-- prettyShader (Shader _ env1 env2 e) = prettyEnv env1 ++ "\n" ++ prettyEnv env2 ++ "\n" ++ wrapMain (concatMap (\x -> "\t" ++ (show x)) e)

-- | Pretty print an env w/ a given function and an Env (aka a list of Functions)
--prettyEnv :: (Func -> String) -> Env -> String
--prettyEnv f ls = concat (intersperse "\n" (filter (not . null) (map f ls)))
-- prettyEnv :: Env -> String
-- prettyEnv e = concat $ intersperse "\n" $ map show e


-- | Pretty globals, generally
-- prettyG :: Type -> String -> String
-- prettyG t n = show t ++ " " ++ n ++ ";"

-- TODO wipe everything above off...

wrapList :: a -> a -> [a] -> [a]
wrapList l r es = [l] ++ es ++ [r]

-- | Constant preface for all shaders
preface :: String
preface = intercalate "\n" [
  "//",
  "// Generated by FDSSL",
  "//",
  "#ifdef GL_ES",
  "precision highp float;",
  "precision highp int;",
  "#endif"] ++ "\n\n"

-- | Represents indentation levels
data Indent = Indent Indent | None

instance Show Indent where
  show (Indent t) = "\t" ++ show t
  show None       = ""

-- | Pretty printer state describes a current indentation level
-- & an ongoing printed program as state
type Printer a = StateT (Indent,[String]) IO a

-- | Marks indentation
indent :: Printer ()
indent = do
  (t,s) <- get
  put $ (Indent t,s)

-- | Marks dedentation
dedent :: Printer ()
dedent = do
  (t,s) <- get
  case t of
    (Indent t) -> put (t,s)
    _       -> return ()

-- | Trim tabs off of strings
trimTabs :: String -> String
trimTabs ls = filter (\c -> c /= '\t') ls

-- | Pop the last evaluted entry on the printer stack
-- Auto-trims tabs off of the entries
pop :: Printer String
pop = do
  (t,(s:ls)) <- get
  put (t,ls)
  -- trim all indentation off
  return $ trimTabs s

-- | ``Pretty Print'' function, allows accumulating an ongoing concrete rep
pPrint :: String -> Printer ()
pPrint s = do
  (t,s') <- get
  put (t,(show t ++ s) : s')

-- | Terminate the last 'statement' expression with a semi-colon
pTermLast :: Printer ()
pTermLast = do
  (t,(s:ls)) <- get
  put (t,(s ++ ";") : ls)

-- | Pretty prints an Env, which is a list of opaque types
prettyEnv' :: Env -> Printer ()
prettyEnv' e = mapM_ (pPrint . show) e

-- | Pretty print parameters
-- TODO, change this into back & forth evaluation & pop runs
prettyParams :: [String] -> String
prettyParams =  concat . wrapList "(" ")" . intersperse ", "

-- | Pretty prints a function
prettyFunc' :: Func -> Printer ()
prettyFunc' (Func name params typ exprs) = do
  let sig = show typ ++ " " ++ name ++ prettyParams (map show params) ++ " {"
  pPrint sig
  indent
  mapM_ prettyStmt exprs
  dedent
  pPrint "}\n"

-- | Pretty print a statement, a standalone expr
prettyStmt :: Expr -> Printer ()
prettyStmt m@(Mut _ _ _) = prettyExpr' m >> pTermLast
prettyStmt c@(Const _ _ _) = prettyExpr' c >> pTermLast
prettyStmt u@(Update _ _) = prettyExpr' u >> pTermLast
prettyStmt o@(Out _ _) = prettyExpr' o >> pTermLast
prettyStmt a@(App _ _) = prettyExpr' a >> pTermLast
-- normally print all others w/out semi-colons afterwards
prettyStmt s = prettyExpr' s

-- | Pretty print an expression
prettyExpr' :: Expr -> Printer ()
-- TODO remove these recursive 'show' calls
prettyExpr' (Mut t n e) = do
  let t' = show t ++ " " ++ n ++ " = "
  -- evaluate & pop the last expr result
  prettyExpr' e
  e' <- pop
  -- add it & continue
  pPrint $ t' ++ e'
prettyExpr' (Const t n e) = do
  -- same as mut
  let t' = "const " ++ show t ++ " " ++ n ++ " = "
  prettyExpr' e
  e' <- pop
  pPrint $ t' ++ e'
prettyExpr' (Update n e) = do
  let t = n ++ " = "
  prettyExpr' e
  e' <- pop
  pPrint $ t ++ e'
prettyExpr' (Out n e) = do
  let t = n ++ " = "
  prettyExpr' e
  e' <- pop
  pPrint $ t ++ e'
prettyExpr' NOp = return ()
prettyExpr' (Branch c t e) = do
  prettyExpr' c
  c' <- pop
  pPrint $ "if (" ++ c' ++ ") {"
  indent
  mapM_ prettyStmt t
  dedent
  pPrint "} else {"
  indent
  mapM_ prettyStmt e
  dedent
  pPrint "}"
prettyExpr' (For i (Just n) e) = do
  i' <- prettyPop i
  pPrint $ "for (int " ++ n ++ " = 0; " ++ n ++ " < " ++ i' ++ "; " ++ n ++ "=" ++ n ++ "+1) {"
  indent
  mapM_ prettyStmt e
  dedent
  pPrint "}"
prettyExpr' (For i Nothing e) = do
  i' <- prettyPop i
  pPrint $ "for (int fdssl_cntr = 0; fdssl_cntr < " ++ i' ++ "; fdssl_cntr = fdssl_cntr" ++ "+1) {"
  indent
  mapM_ prettyStmt e
  dedent
  pPrint "}"
prettyExpr' (SComment s) = pPrint $ "// " ++ s
prettyExpr' (BComment s) = pPrint $ "/*" ++ s ++ "*/"
prettyExpr' (I i) = pPrint $ show i
prettyExpr' (B b) = pPrint $ if b then "true" else "false"
prettyExpr' (F f) = pPrint $ show f
prettyExpr' (D f) = pPrint $ show f
prettyExpr' (V2 (a,b)) = do
  ls <- mapM prettyPop [a,b]
  pPrint $ "vec2" ++ prettyParams ls
prettyExpr' (V3 (a,b,c)) = do
  ls <- mapM prettyPop [a,b,c]
  pPrint $ "vec3" ++ prettyParams ls
prettyExpr' (V4 (a,b,c,d)) = do
  ls <- mapM prettyPop [a,b,c,d]
  pPrint $ "vec4" ++ prettyParams ls
prettyExpr' (Mat4 m) = error "Cannot pretty print mat4, not done yet"
prettyExpr' (Ref r) = pPrint r
prettyExpr' (App n ls) = do
  ls' <- mapM (\x -> prettyExpr' x >> pop) ls
  pPrint $ n ++ prettyParams ls'
prettyExpr' (BinOp b e1 e2) = do
  prettyExpr' e1
  e1' <- pop
  prettyExpr' e2
  e2' <- pop
  pPrint $ intercalate " " [e1', show b, e2']
prettyExpr' (AccessN s n) = pPrint $ s ++ "." ++ n
prettyExpr' (AccessI s i) = pPrint $ s ++ "[" ++ show i ++ "]"
prettyExpr' _ = error "Undefined Expr present! Invalid program"

-- | Pretty printing of an expr followed by a pop to use it
prettyPop :: Expr -> Printer String
prettyPop e = prettyExpr' e >> pop

-- | Pretty prints a shader, which has ins, outs, & a body of expressions
-- This will be represented in an implicit 'main' function
prettyShader' :: Shader -> Printer ()
prettyShader' (Shader _ ins outs exprs) = do
  -- add local shader env (ins)
  prettyEnv' ins
  -- add local shader env (out)
  prettyEnv' outs
  -- prepare a main function for printing out
  pPrint "void main() {"
  indent
  -- evaluate all expressions inside this main shader block
  mapM_ prettyStmt exprs
  dedent
  pPrint "}"

-- | Pretty prints a FDSSL Program for one shader...
prettyProg' :: (Env,Funcs,Shader) -> Printer String
prettyProg' (e,funcs,s) = do
  -- add the preface
  pPrint preface
  -- add the env
  prettyEnv' e
  -- add functions
  mapM_ prettyFunc' (map snd funcs)
  -- add the shader core itself
  prettyShader' s
  -- retrieve & show everything together
  (_,ls) <- get
  return $ intercalate "\n" $ reverse ls

-- | Runs the pretty printer over an FDSSL Program
-- produces a GLSL vertex & fragment shader
runPrettyPrinter :: Prog -> IO (String,String)
runPrettyPrinter (Prog e funcs v f) = do
  -- run for vertex shader
  (a,b) <- runStateT (prettyProg' (e,funcs,v)) (None,[])
  -- run for fragment shader
  (a',b') <- runStateT (prettyProg' (e,funcs,f)) (None,[])
  return (a,a')

-- | temporary...
pretty :: Prog -> IO (String,String)
pretty p = runPrettyPrinter p
