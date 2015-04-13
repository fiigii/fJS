module TypeChecker
       (typeOf)
       where

import Data.List
import Control.Applicative
import Ast

-- TypeChecker
typeOf :: Context -> Ast -> Either String Ty
typeOf ctx (Var i) = getTypeFromContext ctx i
typeOf ctx (Function (name, ty1) t) = TyFun ty1 <$> ty2
  where ty2 = typeOf (extendContext ctx name VariableDef t ty1) t
typeOf ctx (Appliction t1 t2) =
  do ty1 <- typeOf ctx t1
     ty2 <- typeOf ctx t2
     case ty1 of TyFun ty11 ty12 -> if ty2 `isSubtypeOf` ty11
                                    then return ty12
                                    else Left  "parameter type mismatch"
                 _ -> Left "Function needs arrow tpye"

typeOf ctx (IfExpr t1 t2 t3) = if typeOf ctx t1 == Right TyBool
                               then let ty2 = typeOf ctx t2
                                        ty3 = typeOf ctx t3
                                        unionType = joinOf <$> ty2 <*> ty3
                                    in unionType
                               else Left "Conditional type should be Bool"
typeOf ctx (Bool _) = Right TyBool
typeOf ctx (Number _) = Right TyNum
typeOf ctx Unit = Right TyUnit
typeOf ctx (Ref t) = TyRef <$> typeOf ctx t
typeOf ctx (DeRef t) = case typeOf ctx t of Right (TyRef ty) -> Right ty
                                            _ -> Left "Need a Ref Type"
typeOf ctx (Assign t1 t2) =
  let ty1 = typeOf ctx t1
      ty2 = typeOf ctx t2
  in case ty1 of Right (TyRef ty11) ->
                   case ty2 of Right ty22 -> if ty22 `isSubtypeOf` ty11
                                             then Right TyUnit
                                             else Left "Type of assign mismatch"
                               Left error -> Left error
                 _ -> Left "Need a Ref type one the left side of \"=\""
typeOf ctx (Record rs) = TyRecord <$> mapM (\(l, t) -> (,) l <$> typeOf ctx t) rs
typeOf ctx (Access t l) = case tyOfLeft of Right (TyRecord tys) -> lookup l tys
                                           Right _ -> Left "Need a Record"
                                           Left error -> Left error
  where tyOfLeft = typeOf ctx t
        lookup :: String -> TyRecord -> Either String Ty
        lookup s [] = Left $ "There is not \"" ++ s ++ "\" in the record"
        lookup s ((s1, ty1):tys) = if s1 == s then Right ty1 else lookup s tys

typeOf ctx (BinaryExpr op t1 t2) =
  let ty1 = typeOf ctx t1
      ty2 = typeOf ctx t2
  in case ty1 of Right ty11 ->
                   case ty2 of Right ty22  -> if ty11 == TyNum && ty22 == TyNum
                                              then if isLogicOp op
                                                   then Right TyBool
                                                   else if isArithOp op
                                                        then Right TyNum
                                                        else Left "Illeigl Operator"
                                              else Left "Need two Numbers!"
                               Left error -> Left error
                 Left error -> Left error

isLogicOp op = op `elem` [">", "<", ">=", "<=", "==", "!="]

isArithOp op = op `elem` ["+", "-", "*", "/"]


isSubtypeOf :: Ty -> Ty -> Bool
isSubtypeOf tyS tyT = tyS == tyT || subtype tyS tyT
  where subtype _ TyTop = True
        subtype (TyFun tyS1 tyS2) (TyFun tyT1 tyT2) = (tyT1 `isSubtypeOf` tyS1) &&
                                                      (tyS2 `isSubtypeOf` tyT2)
        subtype (TyRecord ss) (TyRecord ts) =
          all (\(li, ti) -> case lookup li ss of Just si -> si `isSubtypeOf` ti
                                                 Nothing -> False) ts
        subtype (TyRef t1) (TyRef t2) = t1 == t2
        subtype _ _ = False

joinOf :: Ty -> Ty -> Ty
joinOf t1 t2 | t1 `isSubtypeOf` t2 = t2
             | t2 `isSubtypeOf` t1 = t1
joinOf (TyRecord ts1) (TyRecord ts2) = let l1s = map fst ts1
                                           l2s = map fst ts2
                                           commonLables = l1s `intersect` l2s
                                           commonFileds =
                                             map (\l -> let (Just t1) = lookup l ts1
                                                            (Just t2) = lookup l ts2
                                                        in (l, joinOf t1 t2)) commonLables
                                       in TyRecord commonFileds
joinOf (TyFun s1 s2) (TyFun t1 t2) =
  case meetOf s1 t1 of Just tt -> TyFun tt (joinOf s2 t2)
                       Nothing -> TyTop
joinOf _ _ = TyTop


meetOf :: Ty -> Ty -> Maybe Ty
meetOf t1 t2 | t1 `isSubtypeOf` t2 = Just t1
             | t2 `isSubtypeOf` t1 = Just t2
meetOf (TyRecord ts1) (TyRecord ts2) =
  let l1s = map fst ts1
      l2s = map fst ts2
      allLables = l1s `union` l2s
      commonLables = l1s `intersect` l2s
      allFields = mapM (\l -> if l `elem` commonLables
                              then do t1 <- lookup l ts1
                                      t2 <- lookup l ts2
                                      m <- meetOf t1 t2
                                      return (l, m)
                              else if l `elem` l1s
                                   then (,) l <$> lookup l ts1
                                   else (,) l <$> lookup l ts2) allLables
  in TyRecord <$> allFields

meetOf (TyFun s1 s2) (TyFun t1 t2) =
  TyFun <$> pure (joinOf s1 t1) <*> meetOf s2 t2

meetOf _ _ = Nothing
                                           
