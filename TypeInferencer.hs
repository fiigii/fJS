module TypeInferencer where

import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (union, (\\))
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (foldlM)

import Ast

type TypeInfer = ExceptT String (State Int)

data Scheme = Forall [String] Type
type Subst = Map.Map String Type
newtype TypeEnv = TypeEnv (Map.Map String Scheme)

class Substitutable a where
  freeTypeVar :: a -> Set.Set String
  applySubst  :: Subst -> a -> a

instance Substitutable Type where
  freeTypeVar (TyVar v) = Set.singleton v
  freeTypeVar (TyList t) = freeTypeVar t
  freeTypeVar (TyFun t1 t2) = freeTypeVar t1 `union` freeTypeVar t2
  freeTypeVar _ = Set.empty

  applySubst subst t@(TyVar v) = fromMaybe t (Map.lookup v subst)
  applySubst subst (TyFun t1 t2) = TyFun (applySubst subst t1) (applySubst subst t2)
  applySubst subst (TyList t) = TyList $ applySubst subst t
  applySubst _ t = t

instance Substitutable Scheme where
  freeTypeVar (Forall vars ty) = freeTypeVar ty \\ Set.fromList vars
  applySubst subst (Forall vars ty) =
    Forall vars (applySubst (foldr Map.delete subst vars) ty)

instance Substitutable a => Substitutable [a] where
  freeTypeVar = foldr (union . freeTypeVar) Set.empty
  applySubst = map . applySubst

instance Substitutable TypeEnv where
  freeTypeVar (TypeEnv env) = freeTypeVar $ Map.elems env
  applySubst subst (TypeEnv env) = TypeEnv $ Map.map (applySubst subst) env

extendEnv :: TypeEnv -> (String, Scheme) -> TypeEnv
extendEnv (TypeEnv env) (x, s) = TypeEnv $ Map.insert x s env 

emptySubst :: Subst
emptySubst = Map.empty

o :: Subst -> Subst -> Subst
s1 `o` s2 = Map.map (applySubst s1) s2 `Map.union` s1

bind :: String -> Type -> TypeInfer Subst
bind a t | t == TyVar a = return emptySubst
         | occurCheck a t = throwError $ "Error: Recursive Type: " ++ a ++ " "++ show t
         | otherwise = return $ Map.singleton a t

occurCheck :: String -> Type -> Bool
occurCheck v t = Set.member v $ freeTypeVar t 

-- infer :: TypeInfer (Subst, Type) -> Either String Scheme
infer t = do res <- evalState (runExceptT (typeInference (TypeEnv Map.empty) t)) 0
             return $ res

fresh :: TypeInfer Type
fresh = do i <- get
           put $ i + 1
           return $ TyVar $ "'" ++ (letters !! i)
  where letters = [1..] >>= flip replicateM ['a'..'z']

unify :: Type -> Type -> TypeInfer Subst
unify (TyFun a r) (TyFun a' r') = do s1 <- unify a a'
                                     s2 <- unify (applySubst s1 r) (applySubst s1 r')
                                     return $ s2 `o` s1

unify (TyVar v) t = bind v t
unify t (TyVar v) = bind v t
unify (TyList t1) (TyList t2) = unify t1 t2
unify TyBool TyBool = return emptySubst
unify TyNum TyNum = return emptySubst
unify TyUnit TyUnit = return emptySubst
unify TyString TyString = return emptySubst
unify t1 t2 = throwError $ "Cannot Match Type: " ++ show t1 ++ " with " ++ show t2

instantiate :: Scheme -> TypeInfer Type
instantiate (Forall vs t) = do vs' <- mapM (const fresh) vs
                               let s =  Map.fromList $ zip vs vs'
                               return $ applySubst s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vs t
  where vs = Set.toList $ freeTypeVar t \\ freeTypeVar env

lookupEnv :: TypeEnv -> String -> TypeInfer (Subst, Type)
lookupEnv (TypeEnv env) v = do
  case Map.lookup v env of Just scheme -> do { t <- instantiate scheme;
                                                    return (emptySubst, t) }
                           Nothing -> throwError $ "Unbound Var: " ++ v 

typeInference :: TypeEnv -> Ast -> TypeInfer (Subst, Type)
typeInference env (Var v) = lookupEnv env v
                                
typeInference env (Function x body) = do
  ty <- fresh
  let newEnv = env `extendEnv` (x, Forall [] ty)
  (s1, t1) <- typeInference newEnv  body
  return (s1, applySubst s1 (TyFun ty t1))
  
typeInference env (Appliction e1 e2) = do
  ty <- fresh
  (s1, t1) <- typeInference env e1
  (s2, t2) <- typeInference (applySubst s1 env) e2
  s3 <- unify (applySubst s2 t1) (TyFun t2 ty)
  return (s3 `o` s2 `o` s1, applySubst s3 ty)

typeInference env (BinaryExpr op e1 e2) = do
  (s1, t1) <- typeInference env e1
  (s2, t2) <- typeInference (applySubst s1 env) e2
  s3 <- unify (applySubst s2 t1) TyNum
  s4 <- unify (applySubst s3 t2) TyNum
  if op `elem` comparer then
    return (s4 `o` s3 `o` s2 `o` s1, TyBool)
    else if op `elem` arith then
         return (s4 `o` s3 `o` s2 `o` s1, TyNum)
         else throwError $ "Type Error on operator: " ++ op

typeInference env (IfExpr e1 e2 e3) = do
  (s1, t1) <- typeInference env e1
  s2 <- unify (applySubst s1 t1) TyBool
  (s3, t3) <- typeInference env e2
  (s4, t4) <- typeInference env e3
  s' <- unify t3 t4
  return (s' `o` s4 `o` s3 `o` s2 `o` s1, applySubst s' t4)

typeInference env (LetExpr binds body) = do
  let inits = map snd binds
  rs1 <- mapM (typeInference env) inits
  let s1s = map fst rs1
      t1s = map snd rs1
      vars = map fst binds
      env' = foldl (\e s -> applySubst s e) env s1s
      ts' = map (generalize env') t1s
      extendedEnv = foldl (\e xt -> extendEnv e xt) env' $ zip vars ts'
  (s', t2) <- typeInference extendedEnv body
  return (foldr o s' s1s, t2)

typeInference env (Letrec [] body) = typeInference env body
typeInference env (Letrec binds body) = do
  tys <- mapM (const fresh) binds
  let vars = map fst binds
      is = map snd binds
      earlyEnv = foldl (\acc (v, ty) -> acc `extendEnv` (v, Forall [] ty)) env $ zip vars tys
  sts <- mapM (\i -> typeInference earlyEnv i) is
  let newSub = foldl1 o $ map fst sts
      newTypes = map snd sts
      env' = applySubst newSub env
      ts' = map (generalize env') newTypes
      addedBinds = zip vars ts'
      extendedEnv = foldl (\acc x -> extendEnv acc x) env' addedBinds
  (s', t2) <- typeInference extendedEnv body
  return (s' `o`  newSub, t2)

typeInference _ (List []) = do ty <- fresh
                               return (emptySubst, TyList ty)
typeInference env (List l) = do
  ty <- fresh
  (s', t') <- foldlM (\(s, t) e -> do { (s', t') <- typeInference (applySubst s env) e;
                                        s'' <- unify (applySubst s' t) t';
                                        return (s'', t') }) (emptySubst, ty) l
  return (s', TyList t')

typeInference env (IsNil e) = do
  (s, t) <- typeInference env e
  ty <- fresh
  s' <- unify t (TyList ty)
  return (s' `o` s, TyBool)

typeInference env (Car e) = do
  (s, t) <- typeInference env e
  ty <- fresh
  s' <- unify t (TyList ty)
  return (s' `o` s, ty)

typeInference env (Cdr e) = do
  (s, t) <- typeInference env e
  ty <- fresh
  s' <- unify t (TyList ty)
  return (s' `o` s, TyList ty)

typeInference env (Cons h d) = do
  (s1, t1) <- typeInference env h
  (s2, t2) <- typeInference (applySubst s1 env) d
  s3 <- unify (applySubst s2 (TyList t1)) t2
  return (s3 `o` s2 `o` s1, t2)

typeInference _ (Number _) = return (emptySubst, TyNum)
typeInference _ (Bool _) = return (emptySubst, TyBool)
typeInference _ (String _) = return (emptySubst, TyString)
typeInference _ Unit = return (emptySubst, TyUnit)

