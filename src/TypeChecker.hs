{-# LANGUAGE ConstraintKinds,FlexibleContexts #-}

--
-- FDSSL Typechecker
--

module TypeChecker where

import Syntax

import Control.Monad.Except   (ExceptT,MonadError,runExceptT,throwError)
import Control.Monad.State    (State,MonadState,get,put,modify,runStateT)
import Control.Monad          (guard, when, sequence, mapM)
import qualified Data.Set as S
import Data.Maybe

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
type TypeChecked m = (
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
  ("gl_BaseInstance", (VertShader,TI))
 ]

-- | Typed param
type ParamType = Type
-- | Return type of a function
type RetType = Type

-- | Builtin general-purpose shader functions
-- note the empty body, we're just interested in the signatures since their definitions are assumed
shaderFuncs :: Funcs
shaderFuncs = [
  (Nothing, Func "sin" [("x",TF)] TF []),
  (Nothing, Func "cos" [("x",TF)] TF [])]

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
lookupFM :: TypeChecked m => String -> Funcs -> m (Maybe ShaderType, Func)
lookupFM n fs = case lookupF n fs of
                  Just f  -> return f
                  Nothing -> throwError $ "Function " ++ n ++ " is not defined"

-- | Lookup an opaque type (uniform) in an env
lookupE :: String -> Env -> Maybe Opaque
lookupE = lookupG opaqueName

-- | Lookup an opaque type (uniform) in the TypeChecked monad
lookupEM :: TypeChecked m => String -> Env -> m Opaque
lookupEM n es = case lookupE n es of
                  Just e  -> return e
                  Nothing -> throwError $ "Function " ++ n ++ " is not defined"

-- | Lookup a local binding in the TypeChecked monad
lookupLM :: TypeChecked m => String -> Locals -> m (Type, Bool)
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
initEnv :: TypeChecked m => Env -> Funcs -> m ()
initEnv e f = put $ Comb e (f ++ shaderFuncs) []

-- | Type check an FDSSL Program
typeCheckProg :: TypeChecked m => Prog -> m Prog
typeCheckProg p@(Prog e f s s')
                   | shaderType s  /= VertShader = throwError "First shader must be a vertex shader"
                   | shaderType s' /= FragShader = throwError "Second shader must be a fragment shader"
                   | outEnv s /= inEnv s'        = throwError $ "Variable types & names passed from the Vertex shader to the Fragment shader must be equivalent: " ++ show (unames $ outEnv s) ++ " and " ++ show (unames $ inEnv s')
                   | otherwise                   = do
                                                     initEnv e f
                                                     typeCheckFuncs
                                                     -- add gl_Position to locals
                                                     modify $ addLocal ("gl_Position",(TV4,True))
                                                     -- type check the vertex shader
                                                     typeCheckShader s
                                                     modify clearLocals
                                                     -- add gl_FragColor to locals
                                                     modify $ addLocal ("gl_FragColor",(TV4,True))
                                                     -- type check the fragment shader
                                                     typeCheckShader s'
                                                     return p

-- | Type check a block, aka a list of exprs
checkBlock :: TypeChecked m => Block -> Type -> m (Type, Maybe ShaderType)
checkBlock [] _     = return (TNull, Nothing)
checkBlock (e:es) t = do
                       (etype, stype) <- checkExpr e
                       when (isImm e && t /= etype) (throwError $ "Final expression of block does not match function return type")
                       checkBlock es t

-- | Type check an FDSSL Function
checkFunc :: TypeChecked m => (Maybe ShaderType, Func) -> m (Maybe ShaderType, Func)
checkFunc (_, f@(Func n p t b)) = do
                                modify (addFuncParams p)
                                (typ, stype) <- checkBlock b t
                                modify clearLocals
                                return (stype, f)

-- | Type check several FDSSL Functions
-- TODO, redundant, down the road we can just do this via mapM and using the 'checkFunc' func above
typeCheckFuncs :: TypeChecked m => m ()
typeCheckFuncs = do
                   (Comb e fs l) <- get

                   f <- mapM checkFunc fs
                   put $ Comb e f l
                   -- Then we can check behavior
                   return ()

-- | Type check an FDSSL Shader
typeCheckShader :: TypeChecked m => Shader -> m ()
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
    False -> throwError $ "Your " ++ show t ++ " shader did not set all output variables: " ++ show (map fst shaderOutputs) ++ "\n" ++ (show $ getLocals ae)
    True  -> case outs == shaderOutputs of
              True  -> return () -- pass, parser already handles this part
              False -> throwError $ "Your " ++ show t ++ " shader set an output variable of incorrect type."

-- | Type checks a given variable/ref
checkVariable :: TypeChecked m => String -> m (Type, Maybe ShaderType)
checkVariable name = do
  (Comb e _ ls) <- get
  case (lookup name ls, lookupE name e, lookup name shaderVals) of
          (Just (t,m), _, _) -> return (t, Nothing)
          (_,Just (Opaque _ t _), _) -> return (t, Nothing)
          (_,_,Just (s,t)) -> return (t, Just s)
          _ -> throwError $ "Variable " ++ name ++ " is not defined"

-- | Type check new bindings, so they do not shadow an existing immutable one
-- and that for mutable bindings the types still match
checkAssign :: TypeChecked m => Bool -> Type -> String -> Expr -> AllEnvs -> m (Type, Maybe ShaderType)
checkAssign mut typ name expr env = do
                                  when (isDefined name env) (throwError $ "Variable " ++ name ++ " already defined")
                                  (etype, stype) <- checkExpr expr
                                  when (etype /= typ) (throwError $ "Variable " ++ name ++ " assigned to expression not matching type")
                                  modify $ addLocal (name,(typ,mut))
                                  return (etype, stype)

-- | Type check an FDSSL expression
checkExpr :: TypeChecked m => Expr -> m (Type, Maybe ShaderType)
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
                                        when (etype /= t) (throwError $ "Variable " ++ name ++ " of type " ++ show t ++ " not the same type as assigned expression of type " ++ show etype)
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

-- TODO the Out case will almost always be for a fresh variable that has never been declared (but is expected upon completion of typechecking a shader)
-- We should revisit this to clean it up to reflect this expected behavior, and it may be appropriate to disallow 'outting' the same output more than once
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
                                     _ -> do
                                       -- first time this has been emitted, add it to the env, and we'll type check it when finishing the shader
                                       (etype, stype) <- checkExpr expr
                                       modify $ addLocal (name,(etype,True))
                                       return (etype, stype)

checkExpr (Branch c e e') = do
                              (etype, stype) <- checkExpr c
                              when (etype /= TB) (throwError $ "Branch conditions must be of type Bool, got type " ++ show etype)
                              es <- mapM checkExpr e
                              es' <- mapM checkExpr e'
                              let (etype, stype) = last es
                              let (etype', stype') = last es'
                              when (etype /= etype') (throwError "Branches must be of the same type")
                              when (stype /= stype') (throwError "Branche shader environments must be of the same type")
                              return (etype, stype)

checkExpr (For c (Just n) b) = do
                                (etype, stype) <- checkExpr c
                                when (etype /= TI) (throwError "For loop count must be an integer")
                                -- verify the introduced loop index is fresh
                                ae <- get
                                when (isDefined n ae == True) (throwError $ "For loop index name, " ++ n ++ " already exists.")
                                es <- mapM checkExpr b
                                let (etype', stype') = last es
                                when (stype /= stype') (throwError "Shader environments must be the same between for loop condition and its body")
                                return (etype', stype')
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
                        (stype, (Func _ params t _)) <- lookupFM n f
                        when (any (uncurry (/=)) (zip (map snd params) (map fst ps))) (throwError $ "Function " ++ n ++ " called with incorrect parameters")
                        return (t, stype)

checkExpr (BinOp o e e') = do
                             (etype, stype) <- checkExpr e
                             (etype', stype') <- checkExpr e'
                             -- type check binops, but allow a special case between mats & vects
                             -- TODO, this is not correct yet, as it allows arbitrary binop usage between mats & vects
                             -- in future work, we need to correctly rewrite binops as overloaded functions definitions, and type check them normally like any other function
                             -- the type list is small enough that we can capture the total acceptable set of operands w/ their operators
                             when (etype /= etype' && not (etype `elem` [TMat4,TV4]) && not (etype' `elem` [TMat4,TV4])) (throwError $ "Expression types do not match in binary operation: " ++ show etype ++ " and " ++ show etype')
                             when (stype /= stype') (throwError $ "Shader types do not match in binary operation: " ++ show stype ++ " and " ++ show stype')
                             let (ptype, rtype) = bopType o
                             when (not $ checkReq ptype etype) (throwError "Binary operator has incorrect parameters ")
                             case toType rtype of
                               Just t  -> return (t, stype)
                               Nothing -> return (etype', stype')
        where
          checkReq (Needs t) t' = t == t'
          checkReq None _ = True
          checkReq (Nott t) t' = t /= t'
          toType (Needs t) = Just t
          toType _ = Nothing
checkExpr (AccessN name n) = do
  -- verify this name is bound
  (t,_) <- checkVariable name
  -- verify the type is suitable for access
  when (t /= TV2 && t /= TV3 && t /= TV4) (throwError $ "Cannot access a named index of non-vector " ++ name ++ " with index " ++ n)
  -- verify the accessing index is one of the 4 suitable named indexes (corresponding to 0-3)
  when (not (n `elem` ["x","y","z","w"])) (throwError $ "Invalid named index " ++ n ++ " used to access vector " ++ name ++ ". Use x,y,z or w.")
  return (TF,Nothing)
checkExpr (AccessI name i) = do
  (t,_) <- checkVariable name
  -- verify the type is suitable for access
  when (t /= TV2 && t /= TV3 && t /= TV4) (throwError $ "Cannot access a named index of non-vector " ++ name ++ " with index " ++ (show i))
  -- verify the sized index is appropriate
  when (t == TV2 && i > 1 || t == TV3 && i > 2 || t == TV4 && i > 3) (throwError $ "Invalid index " ++ show i ++ " used to access " ++ show t ++ " " ++ name)
  return (TF,Nothing)
checkExpr (I _) = return (TI,Nothing)
checkExpr (B _) = return (TB,Nothing)
checkExpr (F _) = return (TF,Nothing)
checkExpr (D _) = return (TF,Nothing)
checkExpr (V2 (e,e')) =  do
                              (_, stype) <- checkSame [e, e']
                              return (TV2, stype) -- We lose type annotation on inner expressions
checkExpr (V3 (e,e',e'')) =  do
                              (_, stype) <- checkSame [e, e', e'']
                              return (TV3, stype)
checkExpr (V4 (e,e',e'',e''')) = do
                              (_, stype) <- checkSame [e, e', e'', e''']
                              return (TV4, stype)

checkExpr (Mat4 _) = return (TMat4, Nothing)
checkExpr (Array e) = checkSame e
checkExpr (Ref name) = checkVariable name
checkExpr NOp = return (TNull, Nothing)

-- | Verify lists of exprs provided to vectors or arrays are homogoenously typed
checkSame :: TypeChecked m => [Expr] -> m (Type, Maybe ShaderType)
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
                    (Left err) -> Left $ "TypeError in Program " ++ s ++ ": " ++ err     -- failed
                    (Right e)  -> case e of
                                    (Left err') -> Left $ "TypeError in Program " ++ s ++ ": " ++ err' -- failed
                                    (Right _)   -> Right (s,p) -- passed
