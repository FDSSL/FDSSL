module TypeChecker where

import Syntax

-- tyep check to make sure applications are good
-- type check to make sure various bin ops and such are good
-- type check calls to functions, if they're undefined fail

-- TODO, finish the type checker
--typecheck :: Prog -> TypeChecked Prog
--typecheck (Prog eGlobal eAttr vs fs) = tcGlobals eGlobal

data TypeChecked a =
  TypeChecked a |
  TypeError String

-- getType :: Expr -> Maybe Type
-- getType (I _) = Just TI
-- getType (B _) = Just TB
-- getType (F _) = Just TF
-- getType (D _) = Just TF
-- getType (V2 _) = Just TV2
-- getType (V3 _) = Just TV3
-- getType (V4 _) = Just TV4
-- getType (Mat4 _) = Just TMat4
-- getType (Array _) = Just TArray
-- getType (Mut t _ _ _) = Just t
-- getType (Const t _ _ _) = Just t
-- getType (Update _ e _) = getType e
-- getType (Out _ e _) = getType e
-- getType (Branch _ e _ _) = getType e
-- getType (SComment _ e) = getType e
-- getType (BComment _ e) = getType e
-- getType (Seq _ e) = getType e
-- getType (For _ _ e _) = getType e
-- getType NOp = Nothing
-- getType (Ref _) = Nothing

-- type Local = [(String, Type)]

-- checkExpr :: Env -> Funcs -> Local -> Expr -> Bool
-- checkExpr e f l (Mut typ name expr nextE) = checkExpr e f l expr && typ == getType expr
-- checkExpr e f l (Const typ name expr nextE) = checkExpr e f l expr && typ == getType expr
-- checkExpr e f l (Update name expr nextE) = checkExpr e f l expr &&
--                                            case (lookup name e, lookup name l) of

-- checkExpr e f l (Out String Expr nextE) = undefined
-- checkExpr e f l (Branch c e1 e1) = undefined
-- checkExpr e f l (For Expr (Maybe String) Expr nextE) = undefined
-- checkExpr e f l (SComment String nextE) = undefined
-- checkExpr e f l (BComment String nextE) = undefined
-- checkExpr e f l (Seq Expr nextE) = undefined
-- checkExpr e f l (App String [Expr] nextE) = undefined
-- checkExpr e f l (BinOp BOp Expr nextE) = undefined
-- checkExpr e f l (AccessN String String) = undefined
-- checkExpr e f l (AccessI String Int) = undefined
