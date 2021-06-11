{-# LANGUAGE
     ConstraintKinds,
     FlexibleContexts
  #-}


module TypeChecker where

import Syntax

import Control.Monad.Except   (ExceptT,MonadError,runExceptT,throwError)
import Control.Monad.State    (State,MonadState,get,put,modify,runStateT)
import Control.Monad          (guard, when, sequence, mapM)
import qualified Data.Set as S
import Data.Maybe

-- tyep check to make sure applications are good
-- type check to make sure various bin ops and such are good
-- type check calls to functions, if they're undefined fail

-- TODO, finish the type checker
--typecheck :: Prog -> TypeChecked Prog
--typecheck (Prog eGlobal eAttr vs fs) = tcGlobals eGlobal

-- | Represents an error message from the type checker
type Error = String

-- | Local binding with a type & whether the binding is mutable
type Mutable  = Bool
type Local    = (String, (Type, Mutable))
type Locals   = [Local]

-- | Represents the env of uniforms, functions, and locally introduced bindings
data AllEnvs = Comb {
  getUniforms :: Env,
  getFuncs    :: Funcs,
  getLocals   :: Locals
}

-- | TypeChecked monad transformer stack
type MonadCheck m = (
  MonadState AllEnvs m,
  MonadError Error m
                    )

-- | Builtin shader values provided by GLSL
shaderVals :: [(String,(ShaderType,Type))]
shaderVals = [
  ("gl_VertexID", (VertShader,TI)),
  ("gl_InstanceID", (VertShader,TI)),
  ("gl_VertexIndex", (VertShader,TI)),
  ("gl_InstanceIndex", (VertShader,TI)),
  ("gl_DrawID", (VertShader,TI)),
  ("gl_BaseVertex",(VertShader,TI)),
  ("gl_BaseInstance", (VertShader,TI)),

  ("gl_Position", (VertShader,TV4)),
  ("gl_FragColor", (VertShader,TV4))
 ]

-- | General lookup function
lookupG :: Eq b => (a -> b) -> b -> [a] -> Maybe a
lookupG f c (x:xs)
    | c == f x = Just x
    | otherwise = lookupG f c xs
lookupG _ _ [] = Nothing

-- | Lookup a function from a list of functions
lookupF :: String -> Funcs -> Maybe (Maybe ShaderType, Func)
lookupF = lookupG (funcName . snd)

-- | Lookup a function in the TypeChecked monad
lookupFM :: MonadCheck m => String -> Funcs -> m (Maybe ShaderType, Func)
lookupFM n fs = case lookupF n fs of
                  Just f  -> return f
                  Nothing -> throwError $ "Function " ++ n ++ " is not defined"

-- | Lookup an opaque type (uniform) in an env
lookupE :: String -> Env -> Maybe Opaque
lookupE = lookupG opaqueName

-- | Lookup an opaque type (uniform) in the TypeChecked monad
lookupEM :: MonadCheck m => String -> Env -> m Opaque
lookupEM n es = case lookupE n es of
                  Just e  -> return e
                  Nothing -> throwError $ "Function " ++ n ++ " is not defined"

-- | Lookup a local binding in the TypeChecked monad
lookupLM :: MonadCheck m => String -> Locals -> m (Type, Bool)
lookupLM n ls = case lookup n ls of
                  Just l -> return l
                  Nothing -> (throwError $ "Local variable " ++ n ++ " is not defined")

-- | Get names from a list of functions
fnames :: Funcs -> [String]
fnames = map (funcName . snd)

-- | Get the names of all uniforms in the env
unames :: Env -> [String]
unames = map opaqueName

-- | Get the names of all locals
lnames :: Locals -> [String]
lnames = map fst

-- | Get the names of all builtin shader functions
snames :: [String]
snames = map fst shaderVals

-- | Predicate for determining whether a name is bound to anything in the TypeChecked env
isDefined :: String -> AllEnvs -> Bool
isDefined s (Comb e f l) = any (elem s) [unames e, lnames l, snames]

-- | Clears local bindings from the TypeChecked env
clearLocals :: AllEnvs -> AllEnvs
clearLocals (Comb u f _) = Comb u f []

-- | Adds locals to the TypeChecked env
addLocals :: Locals -> AllEnvs -> AllEnvs
addLocals ls (Comb e f l) = Comb e f (ls++l)

-- | Adds a single local to the typechecked env
addLocal :: (String, (Type,Bool)) -> AllEnvs -> AllEnvs
addLocal l c = addLocals [l] c

-- | Adds function parameters to the list of locals in the typechecked env
addFuncParams :: [(String,Type)] -> AllEnvs -> AllEnvs
addFuncParams = addLocals . map (\(n, t) -> (n,(t,False)))

-- | Returns a list of any duplicated elements
repeated :: Eq a => [a] -> [a]
repeated = go []
  where
    go ys (n:xs) = case (elem n ys, elem n xs) of
                        (False, True) -> go (n:ys) xs
                        (_,_) -> go ys xs
    go ys []  = ys

-- | Prepares an initial typechecked env
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

-- | Type check an FDSSL Program
typeCheckProg :: MonadCheck m => Prog -> m Prog
typeCheckProg p@(Prog e f s s')
                   | shaderType s  /= VertShader = throwError "First shader must be a vertex shader"
                   | shaderType s' /= FragShader = throwError "Second shader must be a fragment shader"
                   | outEnv s /= inEnv s'        = throwError "Variables passed from the first shader to the second shader must be the equal."
                   | otherwise                   = do
                                                     initEnv e f
                                                     typeCheckFuncs
                                                     typeCheckShader s
                                                     modify clearLocals
                                                     typeCheckShader s'
                                                     return p

-- | Type check a block, aka a list of exprs
checkBlock :: MonadCheck m => Block -> Type -> m (Type, Maybe ShaderType)
checkBlock (e:es) t = do
                       (etype, stype) <- checkExpr e
                       return (TNull, Nothing)
checkBlock (e:[]) t = do
                       (etype, stype) <- checkExpr e
                       when (isImm e && t /= etype) (throwError $ "Final expression of block  does not match function return type")
                       return (TNull, Nothing)

-- | Type check an FDSSL Function
checkFunc :: MonadCheck m => (Maybe ShaderType, Func) -> m (Maybe ShaderType, Func)
checkFunc (_, f@(Func n p t b)) = do
                                modify (addFuncParams p)
                                (typ, stype) <- checkBlock b t
                                modify clearLocals
                                return (stype, f)

-- | Type check several FDSSL Functions
-- TODO, redundant, we should just do this via mapM on the 'checkFunc' func above
typeCheckFuncs :: MonadCheck m => m ()
typeCheckFuncs = do
                   (Comb e fs l) <- get

                   fs <- mapM checkFunc fs
                   put $ Comb e fs l
                   -- Then we can check behavior
                   return ()

-- | Type check an FDSSL Shader
typeCheckShader :: MonadCheck m => Shader -> m ()
typeCheckShader (Shader t ie oe b) = do
  -- collect shader inputs as immutable local bindings
  let shaderInputs = map (\(Opaque o t n) -> (n,(t,False))) ie
  -- collect shader output names to check after evaluating the shader block
  let shaderOutputs = map (\(Opaque o t n) -> (n,(t,True))) oe
  -- add both to the local environment
  mapM_ (modify . addLocal) shaderInputs
  -- typecheck the block of expressions
  _ <- checkBlock b TNull
  -- verify that ALL output mutables have been set in the local environment w/ the proper types
  ae <- get
  let outs = filter (\z -> elem z (getLocals ae)) shaderOutputs
  case length outs == length shaderOutputs of
    False -> throwError $ "Your " ++ show t ++ " shader did not set all output variables."
    True  -> case outs == shaderOutputs of
              True  -> return () -- pass, parser already handles this part
              False -> throwError $ "Your " ++ show t ++ " shader set an output variable of incorrect type."



-- Things to check
--   Redefining variables
--   Check that parameters match function calls
--   Check that binops match
--   Check that AccessI matches length of array
--   Type check assignments

-- | Type check new bindings, so they do not shadow an existing immutable one
-- and that for mutable bindings the types still match
checkAssign :: MonadCheck m => Bool -> Type -> String -> Expr -> AllEnvs -> m (Type, Maybe ShaderType)
checkAssign mut typ name expr env = do
                                  when (isDefined name env) (throwError $ "Variable " ++ name ++ " already defined")
                                  (etype, stype) <- checkExpr expr
                                  when (etype /= typ) (throwError $ "Variable " ++ name ++ " assigned to expression not matching type")
                                  modify $ addLocal (name,(typ,mut))
                                  return (etype, stype)

-- | Type check an FDSSL expression
checkExpr :: MonadCheck m => Expr -> m (Type, Maybe ShaderType)
checkExpr (Mut typ name expr)   = get >>= checkAssign True typ name expr
checkExpr (Const typ name expr) = get >>= checkAssign False typ name expr
-- update of an existing binding
checkExpr (Update name expr)    = do
                                    env@(Comb e _ l) <- get
                                    when (not $ isDefined name env) (throwError $ "Variable " ++ name ++ " not defined")
                                    when (lookupE name e /= Nothing) (throwError $ "Cannot assign opaque variable " ++ name)
                                    -- attempt to find this in the local bindings & builtins
                                    case lookup name l of
                                      -- no binding found
                                      Nothing -> throwError $ "Variable " ++ name ++ " not found"
                                      -- local binding found
                                      Just (t,m)     -> do
                                        when (m == False) (throwError $ "Variable " ++ name ++ " cannot be modified")
                                        (etype, stype) <- checkExpr expr
                                        when (etype /= t) (throwError $ "Variable " ++ name ++ " not the same type as assigned expression.")
                                        return (t, stype)
                                      -- built-in variable binding found, correlates to gl_pos or gl_col
                                      -- TODO no clue where to fix it from here....
                                      -- I added a 2-tuple here to try and handle local lookups AND shader val lookups to account for setting of things like glPos & glFragColor
                                      -- we need to add them to the TC env as we find them, and then the logic I added into `typeCheckShader` verifies that the outputs were set in the block and they match what we're expecting type-wise
                                      -- (_,Just (t,m))     -> do
                                      --   when (m == False) (throwError $ "Variable " ++ name ++ " cannot be modified")
                                      --   (etype, stype) <- checkExpr expr
                                      --   when (etype /= t) (throwError $ "Variable " ++ name ++ " not the same type as assigned expression.")
                                      --   return (t, stype)

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

checkExpr (Branch c e e') = do
                              (etype, stype) <- checkExpr c
                              when (etype /= TB) (throwError "Branch conditions must be of type Bool")
                              es <- mapM checkExpr e
                              es' <- mapM checkExpr e'
                              let (etype, stype) = last es
                              let (etype', stype') = last es'
                              when (etype /= etype') (throwError "Branches must be of the same type")
                              when (stype /= stype') (throwError "Branche shader environments must be of the same type")
                              return (etype, stype)

checkExpr (For c (Just n) b) = undefined
checkExpr (For c Nothing b) = do
                                (etype, stype) <- checkExpr c
                                when (etype /= TI) (throwError "For loop count must be an integer")
                                es <- mapM checkExpr b
                                let (etype', stype') = last es
                                when (stype /= stype') (throwError "Shader environments must be the same between for loop condition and its body")
                                return (etype', stype')
checkExpr (SComment _) = return (TNull, Nothing)
checkExpr (BComment _) = return (TNull, Nothing)
checkExpr (App n p) = do
                        (Comb e f l) <- get
                        ps <- mapM checkExpr p
                        (stype, (Func _ p t _)) <- lookupFM n f
                        when (any (uncurry (/=)) (zip (map snd p) (map fst ps))) (throwError $ "Function " ++ n ++ " called with incorrect parameters")
                        return (t, stype)

checkExpr (BinOp _ e e') = do
                             (etype, stype) <- checkExpr e
                             (etype', stype') <- checkExpr e'
                             when (etype /= etype') (throwError $ "Expression types to not match in binrary operation")
                             when (stype /= stype') (throwError $ "Shader types to not match in binrary operation")
                             return (etype, stype)

checkExpr (AccessN name member) = do
                                    (Comb e _ ls) <- get
                                    case (lookup name ls, lookupE name e, lookup name shaderVals) of
                                         (Just (t,m), _, _) -> return (t, Nothing)
                                         (_,Just (Opaque _ t _), _) -> return (t, Nothing)
                                         (_,_,Just (s,t)) -> return (t, Just s)
                                         _ -> throwError $ "Collection " ++ name ++ " is not defined"

checkExpr (AccessI name idx) = do
                                 (Comb e _ ls) <- get
                                 case (lookup name ls, lookupE name e, lookup name shaderVals) of
                                         (Just (t,m), _, _) -> return (t, Nothing)
                                         (_,Just (Opaque _ t _), _) -> return (t, Nothing)
                                         (_,_,Just (s,t)) -> return (t, Just s)
                                         _ -> throwError $ "Collection " ++ name ++ " is not defined"


checkExpr (I _) = return (TI,Nothing)
checkExpr (B _) = return (TB,Nothing)
checkExpr (F _) = return (TF,Nothing)
checkExpr (D _) = return (TF,Nothing)
checkExpr (V2 (e,e')) =  do
                              (_, stype) <- checkSame [e, e']
                              return (TV2, stype)
checkExpr (V3 (e,e',e'')) =  do
                              (_, stype) <- checkSame [e, e', e'']
                              return (TV3, stype)
checkExpr (V4 (e,e',e'',e''')) = do
                              (_, stype) <- checkSame [e, e', e'', e''']
                              return (TV4, stype)

checkExpr (Mat4 _) = return (TMat4, Nothing)
checkExpr (Array e) = checkSame e

-- | Verify lists of exprs provided to vectors or arrays are homogoenously typed
checkSame :: MonadCheck m => [Expr] -> m (Type, Maybe ShaderType)
checkSame es = do
                   ps <- mapM checkExpr es
                   let ptypes = map fst ps
                   let stypes = map snd ps
                   when (S.size (S.fromList ptypes) /= 1) (throwError "Associated (Vector or Array) expressions must be all the same types.")
                   when (S.size (S.fromList stypes) /= 1) (throwError "Associated (Vector or Array) epressions must be all the same shader types.")
                   return (head ptypes, head stypes)

-- | Runs the type checker on a named prog, returning an error or the same named prog
runTypeChecker :: (String,Prog) -> Either Error (String,Prog)
runTypeChecker (s,p) = case runExceptT $ runStateT (typeCheckProg p) (Comb [] [] []) of
                    (Left err) -> Left $ "Program " ++ s ++ ": " ++ err     -- failed
                    (Right e)  -> case e of
                                    (Left err') -> Left $ "Program " ++ s ++ ": " ++ err' -- failed
                                    (Right _)   -> Right (s,p) -- passed
