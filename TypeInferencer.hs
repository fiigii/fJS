{-# LANGUAGE FlexibleInstances #-}
module TypeInferencer
       (typeInference)
       where

import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Set (union, (\\))
import Data.List (nub)
import Control.Applicative
import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Identity

import Ast

type Subst = Map.Map String Type
type Solve a = ExceptT String Identity a
type Constraint = (Type, Type)
type TypeInfer = RWST TypeEnv [Constraint] Int (ExceptT String Identity)
type Unifier = (Subst, [Constraint])
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

instance Substitutable Constraint where
  freeTypeVar (t1, t2) = freeTypeVar t1 `union` freeTypeVar t2
  applySubst s (t1, t2) = (applySubst s t1, applySubst s t2)

multiExtendEnv:: TypeEnv -> [(String, Scheme)] -> TypeEnv
multiExtendEnv (TypeEnv env) ls =
  TypeEnv $ foldr (\(v, s) acc -> Map.insert v s acc) env ls

remove :: TypeEnv -> String -> TypeEnv
remove (TypeEnv env) v = TypeEnv $ Map.delete v env

emptyEnv :: TypeEnv
emptyEnv = TypeEnv Map.empty


closeOver :: Type -> Scheme
closeOver = normalize <$> generalize emptyEnv

normalize :: Scheme -> Scheme
normalize (Forall _ ty) = Forall (map snd ord) (normalizedType ty)
  where ord = zip (nub $ Set.toList $ freeTypeVar ty) letters
        normalizedType (TyFun t1 t2) = TyFun (normalizedType t1) (normalizedType t2)
        normalizedType (TyList t) = TyList $ normalizedType t
        normalizedType (TyVar v) = TyVar $ fromJust $ lookup v ord
        normalizedType t = t

emptySubst :: Subst
emptySubst = Map.empty

o :: Subst -> Subst -> Subst
s1 `o` s2 = Map.map (applySubst s1) s2 `Map.union` s1

occurCheck :: String -> Type -> Bool
occurCheck v t = Set.member v $ freeTypeVar t 

fresh :: TypeInfer Type
fresh = do i <- get
           put $ i + 1
           return $ TyVar $ letters !! i

letters :: [String]
letters = [1..] >>= flip replicateM ['a'..'z']

instantiate :: Scheme -> TypeInfer Type
instantiate (Forall vs t) = do vs' <- mapM (const fresh) vs
                               let s =  Map.fromList $ zip vs vs'
                               return $ applySubst s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Forall vs t
  where vs = Set.toList $ freeTypeVar t \\ freeTypeVar env

-- The Contraint-based implementation

initEnv :: TypeEnv
initEnv = TypeEnv Map.empty

typeInference :: Ast -> Either String Scheme
typeInference t = do
  (ty, cs) <- runInfer initEnv $ infer t
  su <- runSolve cs
  return $ closeOver $ applySubst su ty

runInfer :: TypeEnv -> TypeInfer Type -> Either String (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env 0

lookupEnv :: String -> TypeInfer Type
lookupEnv v = do
  (TypeEnv env) <- ask
  case Map.lookup v env of Just scheme -> instantiate scheme
                           Nothing -> throwError $ "Unbound Var: " ++ v

mustEqual :: Type -> Type -> TypeInfer ()
mustEqual t1 t2 = tell [(t1, t2)]

withEnv :: TypeInfer a -> [(String, Scheme)] -> TypeInfer a
withEnv m new = local newScope m
  where newScope e = multiRemove e `multiExtendEnv` new
        multiRemove e = foldl remove e vars
        vars = map fst new
infer :: Ast -> TypeInfer Type
infer (Var v) = lookupEnv v
infer (Function x body) = do
  ty <- fresh
  t <- infer body `withEnv` [(x, Forall [] ty)]
  return $ TyFun ty t
  
infer (Application fun arg) = do
  tf <- infer fun
  ta <- infer arg
  tr <- fresh
  tf `mustEqual` TyFun ta tr
  return tr
  
infer (LetExpr binds body) = do
  env <- ask
  let inits = map snd binds
      vars  = map fst binds
  initTypes <- mapM infer inits
  let schemes = map (generalize env) initTypes
  infer body `withEnv` zip vars schemes
  
infer (LetRec [] body) = infer body
infer (LetRec binds body) = do
  env <- ask
  tvs <- mapM (const fresh) binds
  let vars = map fst binds
      inits = map snd binds
      emptySchemes = zip vars $ map (Forall []) tvs
      earlyEnv = env `multiExtendEnv` emptySchemes
  initTypes <- mapM (\e -> infer e `withEnv` emptySchemes) inits
  mapM_ (uncurry mustEqual) $ zip tvs initTypes
  --let schemes = map (generalize earlyEnv) initTypes
  let schemes = map (Forall []) initTypes
  infer body `withEnv` zip vars schemes
  
infer (IfExpr e1 e2 e3) = do
  t1 <- infer e1
  t2 <- infer e2
  t3 <- infer e3
  t1 `mustEqual` TyBool
  t2 `mustEqual` t3
  return t2
  
infer (BinaryExpr op e1 e2) = do
  t1 <- infer e1
  t2 <- infer e2
  t1 `mustEqual` TyNum
  t2 `mustEqual` TyNum
  if op `elem` arith then return TyNum
    else if op `elem` comparer then return TyBool
         else throwError $ "Type Error on operator: " ++ op
              
infer (List []) = TyList <$> fresh
infer (List l) = do
  ty <- infer $ head l
  types <- mapM infer $ tail l
  mapM_ (mustEqual ty) types
  return $ TyList ty
  
infer (IsNil e) = do
  ty <- infer e
  tv <- fresh
  ty `mustEqual` TyList tv
  return TyBool
  
infer (Car e) = do
  ty <- infer e
  tv <- fresh
  ty `mustEqual` TyList tv
  return tv
  
infer (Cdr e) = do
  ty <- infer e
  tv <- fresh
  ty `mustEqual` TyList tv
  return ty
  
infer (Cons e l ) = do
  te <- infer e
  tl <- infer l
  tl `mustEqual` TyList te
  return tl
  
infer (Number _) = return TyNum
infer (Bool _) = return TyBool
infer (String _) = return TyString
infer Unit = return TyUnit
infer _ = throwError "Unknow Term"

bind :: String -> Type -> Solve Subst
bind a t | t == TyVar a = return emptySubst
         | occurCheck a t = throwError $ "Error: Recursive Type: " ++ a ++ " "++ show t
         | otherwise = return $ Map.singleton a t

unify :: Type -> Type -> Solve Subst
unify t1 t2 | t1 == t2 = return emptySubst
unify (TyVar v) t = v `bind` t
unify t (TyVar v) = v `bind` t
unify (TyList t1) (TyList t2) = unify t1 t2
unify (TyFun ta1 tr1) (TyFun ta2 tr2) = unifyMany [ta1, tr1] [ta2, tr2]
unify t1 t2 = throwError $ "Cannot Match Type: " ++ show t1 ++ " with " ++ show t2

unifyMany :: [Type] -> [Type] -> Solve Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
  sub <- unify t1 t2
  sub' <- unifyMany (applySubst sub ts1) (applySubst sub ts2)
  return $ sub' `o` sub
unifyMany t1 t2 = throwError $ "Cannot Match Type: " ++ show t1 ++ " with " ++ show t2

solver :: Unifier -> Solve Subst
solver (su, []) = return su
solver (su, ((t1, t2) : cs')) = do
  su' <- unify t1 t2
  solver (su' `o` su, applySubst su' cs')

runSolve :: [Constraint] -> Either String Subst
runSolve cs = runIdentity $ runExceptT $ solver (emptySubst, cs)
