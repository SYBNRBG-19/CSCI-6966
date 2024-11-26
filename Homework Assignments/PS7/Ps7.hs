module Ps7 where
import Control.Exception
import Data.Typeable
import Data

-- Errors.
data ToImplement = ToImplement deriving (Show, Typeable)
instance Exception ToImplement

-- Substitutions.
substType :: Type -> Subst -> Type
substType (TBase bt) _ = TBase bt
substType (TVar v) subst = case lookup v subst of
    Just t -> substType t subst
    Nothing -> TVar v
substType (TArrow t1 t2) sbs = TArrow (substType t1 sbs) (substType t2 sbs)

substEnv::TEnv -> Subst -> TEnv
substEnv env subst = [(var, substType ty subst) | (var, ty) <- env]

-- Occurs check and unification.
occursIn :: Type -> Type -> Bool
occursIn needle (TBase _) = False
occursIn needle haystack@(TVar _) = needle == haystack
occursIn needle (TArrow t1 t2) = occursIn needle t1 || occursIn needle t2

-- Bind a type variable to a type, checking for circularity. 
varBind :: TVar -> Type -> Subst
varBind v t
    | t == TVar v = []
    | occursIn (TVar v) t = throw TypeCircularity
    | otherwise = [(v, t)]

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = [(v, substType t s2) | (v, t) <- s1] ++ s2

unify :: Type -> Type -> Subst
unify (TBase b1) (TBase b2)
    | b1 == b2 = []
    | otherwise = throw (TypeMismatch (TBase b1) (TBase b2))
unify (TVar v) t = varBind v t
unify t (TVar v) = varBind v t
unify (TArrow l1 r1) (TArrow l2 r2) =
    let s1 = unify l1 l2
        s2 = unify (substType r1 s1) (substType r2 s1)
    in composeSubst s2 s1
unify t1 t2 = throw (TypeMismatch t1 t2)

-- Type inference.
inferTypes :: TEnv -> Integer -> Exp -> (Subst, Type, Integer)
-- Variable
inferTypes tenv index (EVar var) =
    case lookup var tenv of
        Just ty -> ([], ty, index)
        Nothing -> throw (UnboundVar var)
-- Lambda abstraction
inferTypes tenv index (ELambda v body) =
    let
        (tvar, index1) = freshTVar index
        tenv' = (v, tvar) : tenv
        (s1, tyBody, index2) = inferTypes tenv' index1 body
        tyVarSubst = substType tvar s1
    in
        (s1, TArrow tyVarSubst tyBody, index2)
-- Application
inferTypes tenv index (EApp fn arg) =
    let
        (s1, tyFn, index1) = inferTypes tenv index fn
        tenv1 = substEnv tenv s1
        (s2, tyArg, index2) = inferTypes tenv1 index1 arg
        s3 = composeSubst s2 s1
        tyFn' = substType tyFn s2
        (tRes, index3) = freshTVar index2
        s4 = unify tyFn' (TArrow tyArg tRes)
        s = composeSubst s4 s3
        tyResult = substType tRes s
    in
        (s, tyResult, index3)
-- Numeric literal
inferTypes _ index (EPrim (PNum _)) = ([], intType, index)
-- Boolean literal
inferTypes _ index (EPrim (PBool _)) = ([], boolType, index)

-- Generate fresh type variables.
freshTVar :: Integer -> (Type, Integer)
freshTVar idx = (TVar ("__t" ++ show idx), idx + 1)

-- Pretty printing.
canonicalize :: Exp -> String
canonicalize exp =
    let
        (s, ty, _) = inferTypes [] 0 exp
        ty' = substType ty s
        vars = collectTVars ty'
        renaming = renameTVars vars
        ty'' = substType ty' renaming
    in
        prettyPrintType ty''

-- Canonicalization.
collectTVars :: Type -> [TVar]
collectTVars (TBase _) = []
collectTVars (TVar v) = [v]
collectTVars (TArrow t1 t2) = collectTVars t1 ++ collectTVars t2

unique :: Eq a => [a] -> [a]
unique lst = uniqueHelper lst []
  where
    uniqueHelper [] _ = []
    uniqueHelper (x:xs) seen
      | x `elem` seen = uniqueHelper xs seen
      | otherwise = x : uniqueHelper xs (x:seen)

renameTVars :: [TVar] -> Subst
renameTVars vars = 
    [(v, TVar ("t" ++ show idx)) | (v, idx) <- zip (unique vars) [1..]]

prettyPrintType :: Type -> String
prettyPrintType ty = ppType ty False
  where
    ppType (TBase BTInt) _ = "Int"
    ppType (TBase BTBool) _ = "Bool"
    ppType (TVar v) _ = v
    ppType (TArrow t1 t2) outer =
        let
            left = ppType t1 True
            right = ppType t2 False
            s = left ++ " -> " ++ right
        in
            if outer then "(" ++ s ++ ")" else s
