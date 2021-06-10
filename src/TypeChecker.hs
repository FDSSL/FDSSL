{-# LANGUAGE
     ConstraintKinds,
     FlexibleContexts
  #-}


module TypeChecker where

import Syntax

import Control.Monad.Except   (ExceptT,MonadError,runExceptT,throwError)
import Control.Monad.State    (State,MonadState,get,put,modify)
import Control.Monad          (guard, when)

-- tyep check to make sure applications are good
-- type check to make sure various bin ops and such are good
-- type check calls to functions, if they're undefined fail

-- TODO, finish the type checker
--typecheck :: Prog -> TypeChecked Prog
--typecheck (Prog eGlobal eAttr vs fs) = tcGlobals eGlobal

type Error = String

type Locals = [(String, Type)]

data AllEnvs = Comb {
  getUniforms :: Env,
  getFuncs :: Funcs,
  getLocals :: Locals
}

type MonadCheck m = (
  MonadState AllEnvs m,
  MonadError Error m
                    )
type CheckM a = ExceptT Error (State AllEnvs) a

shaderVals :: [(String,(ShaderType,Type))]
shaderVals = [
  ("gl_VertexID", (VertShader,TI)),
  ("gl_InstanceID", (VertShader,TI)),
  ("gl_VertexIndex", (VertShader,TI)),
  ("gl_InstanceIndex", (VertShader,TI)),
  ("gl_DrawID", (VertShader,TI)),
  ("gl_BaseVertex",(VertShader,TI)),
  ("gl_BaseInstance", (VertShader,TI))
 ]

fnames :: Funcs -> [String]
fnames = map (funcName . snd)

unames :: Env -> [String]
unames = map opaqueName

lnames :: Locals -> [String]
lnames = map fst

snames :: [String]
snames = map fst shadeVals

isDefined :: AllEnvs -> String -> Bool
isDefined (Comb e f l) s = any (elem s) [unames e, fnames f, lnames l, snames]

clearLocals :: AllEnvs -> AllEnvs
clearLocals (Comb u f _) = Comb u f []

addLocals :: Locals -> AllEnvs -> AllEnvs
addLocals ls (Comb e f l) = Comb e f (ls++l)

addLocal :: (String, Type) -> AllEnvs -> AllEnvs
addLocal l c = addLocals [l] c

repeated :: Eq a => [a] -> [a]
repeated = go []
  where
    go ys (n:xs) = case (elem n ys, elem n xs) of
                        (False, True) -> go (n:ys) xs
                        (_,_) -> go ys xs
    go ys []  = ys

initEnv :: MonadCheck m => Env -> Funcs -> m ()
initEnv e f = do
                 -- let funcs = map (funcName . snd) f
                 -- let uniforms = map opaqueName e
                 -- let shared = unique funcs uniforms

                 -- when (shared /= []) (throwError ("Name overlap between defined functions and uniform variables: " ++ show shared))

                 -- let funcs = repeated funcs
                 -- let uniforms = repeated uniforms

                 -- when (funcs /= []) (throwError ("Multiple function definitions: " ++  show funcs))
                 -- when (uniforms /= []) (throwError ("Multiple uniform definition: " ++ show uniforms))

                 put $ Comb e f []



typeCheckProg :: MonadCheck m => Prog -> m Prog
typeCheckProg p@(Prog e f s s')
                   | shaderType s  /= FragShader = throwError "First shader must be a fragment shader"
                   | shaderType s' /= VertShader = throwError "Second shader must be a vertex shader"
                   | outEnv s /= inEnv s'        = throwError "Variables passed from the first shader to the second shader must be the equal."
                   | otherwise                   = do
                                                     initEnv e f
                                                     typeCheckFuncs
                                                     typeCheckShader s
                                                     modify clearLocals
                                                     typeCheckShader s'
                                                     return p


checkBlock :: MonadCheck m => Block -> Type -> m (Type, Maybe ShaderType)
checkBlock (e:es) t = do
                       (etype, stype) <- checkExpr e
                       return Nothing
checkBody (e:[]) t = do
                       typ <-  e
                       when (isImm e && t /= typ) (throwError "Final expression " ++ show e ++ " does not match function return type")


checkFunc :: MonadCheck m => Func -> m (Maybe ShaderType, Func)
checkFunc f@(Func n p t b) = do
                                modify (addLocals p)
                                stype <- checkBody b t
                                modify clearLocals
                                return (stype, f)


checkFuncs :: MonadCheck m => [(Maybe ShaderType, Func)] -> m [(Maybe ShaderType, Func)]
checkFuncs ((_, f):fs) = do
                             pair <- checkFunc f
                             pairs <- checkFuncs fs
                             return (pair:pairs)
checkFuncs []          = return []


typeCheckFuncs :: MonadCheck m => m ()
typeCheckFuncs = do
                   (Comb e fs l) <- get

                   fs <- checkFuncs fs
                   put $ Comb e fs l
                   -- Then we can check behavior
                   return ()


typeCheckShader :: MonadCheck m => Shader -> m ()
typeCheckShader (Shader t ie oe b) = undefined



-- Things to check
--   Redefining variables
--   Check that parameters match function calls
--   Check that binops match
--   Check that AccessI matches length of array
--   Type check assignments


checkExpr :: MonadCheck m => Expr -> m (Type, Maybe ShaderType)
checkExpr (Mut typ name expr) = do
                                  env <- get
                                  when (isDefined env name) (throwError "Mutable variable " ++ name ++ " already defined")
                                  (etype, stype) <- checkExpr expr
                                  when (etype /= typ) (throwError "Variable " ++ name ++ " assigned to expression of type " ++ etype)
                                  addLocal (name,typ)
                                  return (etype, stype)
checkExpr (Const typ name expr) = checkExpr e f l expr && typ == getType expr
checkExpr (Update name expr) = checkExpr expr &&
                                     case (lookup name e, lookup name l) of

checkExpr (Out String Expr) = undefined
checkExpr (Branch c e1 e1) = undefined
checkExpr (For Expr (Maybe String) Expr) = undefined
checkExpr (SComment String) = undefined
checkExpr (BComment String) = undefined
checkExpr (Seq Expr) = undefined
checkExpr (App String [Expr]) = undefined
checkExpr (BinOp BOp Expr) = undefined
checkExpr (AccessN String String) = undefined
checkExpr (AccessI String Int) = undefined
