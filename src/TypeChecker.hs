{-# LANGUAGE
     ConstraintKinds,
     FlexibleContexts
  #-}


module TypeChecker where

import Syntax

import Control.Monad.Except   (ExceptT,MonadError,runExceptT,throwError)
import Control.Monad.State    (State,MonadState,get,put,modify)
import Control.Monad          (guard, when, sequence, mapM)
import Data.Maybe

-- tyep check to make sure applications are good
-- type check to make sure various bin ops and such are good
-- type check calls to functions, if they're undefined fail

-- TODO, finish the type checker
--typecheck :: Prog -> TypeChecked Prog
--typecheck (Prog eGlobal eAttr vs fs) = tcGlobals eGlobal

type Error = String

type Locals = [(String, (Type, Bool))]

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

lookupG :: Eq b => (a -> b) -> b -> [a] -> Maybe a
lookupG f c (x:xs)
    | c == f x = Just x
    | otherwise = lookupG f c xs
lookupG _ _ [] = Nothing

lookupF :: String -> Funcs -> Maybe (Maybe ShaderType, Func)
lookupF = lookupG (funcName . snd)

lookupE :: String -> Env -> Maybe Opaque
lookupE = lookupG opaqueName

fnames :: Funcs -> [String]
fnames = map (funcName . snd)

unames :: Env -> [String]
unames = map opaqueName

lnames :: Locals -> [String]
lnames = map fst

snames :: [String]
snames = map fst shaderVals

isDefined :: String -> AllEnvs -> Bool
isDefined s (Comb e f l) = any (elem s) [unames e, lnames l, snames]

clearLocals :: AllEnvs -> AllEnvs
clearLocals (Comb u f _) = Comb u f []

addLocals :: Locals -> AllEnvs -> AllEnvs
addLocals ls (Comb e f l) = Comb e f (ls++l)

addLocal :: (String, (Type,Bool)) -> AllEnvs -> AllEnvs
addLocal l c = addLocals [l] c

addFuncParams :: [(String,Type)] -> AllEnvs -> AllEnvs
addFuncParams = addLocals . map (\(n, t) -> (n,(t,False)))

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
                       return (TNull, Nothing)
checkBlock (e:[]) t = do
                       (etype, stype) <- checkExpr e
                       when (isImm e && t /= etype) (throwError $ "Final expression of block  does not match function return type")
                       return (TNull, Nothing)


checkFunc :: MonadCheck m => (Maybe ShaderType, Func) -> m (Maybe ShaderType, Func)
checkFunc (_, f@(Func n p t b)) = do
                                modify (addFuncParams p)
                                (typ, stype) <- checkBlock b t
                                modify clearLocals
                                return (stype, f)


typeCheckFuncs :: MonadCheck m => m ()
typeCheckFuncs = do
                   (Comb e fs l) <- get

                   fs <- mapM checkFunc fs
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

checkAssign :: MonadCheck m => Bool -> Type -> String -> Expr -> AllEnvs -> m (Type, Maybe ShaderType)
checkAssign mut typ name expr env = do
                                  when (isDefined name env) (throwError $ "Variable " ++ name ++ " already defined")
                                  (etype, stype) <- checkExpr expr
                                  when (etype /= typ) (throwError $ "Variable " ++ name ++ " assigned to expression not matching type")
                                  modify $ addLocal (name,(typ,mut))
                                  return (etype, stype)

checkExpr :: MonadCheck m => Expr -> m (Type, Maybe ShaderType)
checkExpr (Mut typ name expr)   = get >>= checkAssign True typ name expr
checkExpr (Const typ name expr) = get >>= checkAssign False typ name expr
checkExpr (Update name expr)    = do
                                    env@(Comb e _ l) <- get
                                    when (not $ isDefined name env) (throwError $ "Variable " ++ name ++ " not defined")
                                    when (lookupE name e /= Nothing) (throwError $ "Cannot assign opaque variable " ++ name)
                                    case lookup name l of
                                      Nothing -> throwError "Variable not found"
                                      Just (t,m) -> do
                                                        when (m == False) (throwError $ "Variable " ++ name ++ " cannot be modified")
                                                        (etype, stype) <- checkExpr expr
                                                        when (etype /= t) (throwError $ "Variable " ++ name ++ " not the same type as assigned expression.")
                                                        return (t, stype)

checkExpr (Out name expr)      = do
                                   (Comb e _ _) <- get
                                   case (lookupE name e, lookup name shaderVals) of
                                     (Just (Opaque otype typ n), Nothing) -> do
                                       (etype, stype) <- checkExpr expr
                                       when (elem otype [Uniform, Attribute]) (throwError $ "Cannot modify opaque variable " ++ n)
                                       when (etype /= typ) (throwError $ "Variable " ++ name ++ " does not match out type.")
                                       return (etype, stype)
                                     (Nothing,                   Just (stype, svtype)) -> do
                                       (etype, estype) <- checkExpr expr
                                       when (etype /= svtype) (throwError $ "Variable " ++ name ++ " does not match out type.")
                                       when (any (stype /=) estype) (throwError $ "Variable " ++ name ++ " does not match out type.")
                                       return (etype, Just stype)
                                     _ -> throwError $ "Variable " ++ name ++ " can not be set as an out."



checkExpr (Branch c e e') = undefined
checkExpr (For c (Just n) b) = undefined
checkExpr (For c Nothing b) = undefined
checkExpr (SComment _) = return (TNull, Nothing)
checkExpr (BComment _) = return (TNull, Nothing)
checkExpr (App n p) = do
                        (Comb e f l) <- get
                        ps <- mapM checkExpr p
                        return (TNull, Nothing)

checkExpr (BinOp o e e') = undefined
checkExpr (AccessN name member) = undefined
checkExpr (AccessI name idx) = undefined
