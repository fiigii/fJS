module Ast where

import Data.List (find)

data Ast  = Var String
          | Function (String, Ty) Ast
          | Appliction Ast Ast
          | IfExpr Ast Ast Ast
          | Bool Bool
          | String String
          | Record TermRecord
          | Access Ast String
          | Ref Ast
          | DeRef Ast
          | Assign Ast Ast
          | Location Int
          | Unit
          | Number Int
          | BinaryExpr String Ast Ast
          | Closure Ast Context
          deriving (Show, Eq)

type TermRecord = [(String, Ast)]

data Ty = TyFun Ty Ty
        | TyBool
        | TyRecord TyRecord
        | TyRef Ty
        | TyUnit
        | TyNum
        | TyTop
        deriving (Eq)

type TyRecord = [(String, Ty)]

instance Show Ty where
  show (TyFun t1@(TyFun _ _) t2) = " (" ++ show t1 ++ ") ->" ++ show t2
  show (TyFun t1 t2) = show t1 ++ " ->" ++ show t2
  show TyBool = " Bool"
  show TyNum = " Number"
  show TyUnit = " Unit"
  show TyTop = " Top"
  show (TyRef t@(TyRef _)) = " Ref(" ++ show t ++ " )"
  show (TyRef t@(TyFun _ _)) = " Ref(" ++ show t ++ " )"
  show (TyRef t) = " Ref" ++ show t
  show (TyRecord ts) = "{ " ++
                       foldl
                         (\acc (s, tt) -> acc ++ s ++ ":" ++ show tt ++ ", ")
                         ""
                         ts ++
                         "}"

type Context = [(Binding)]

data Binding = VarBind String BindingForm Ast Ty
             deriving (Eq, Show)

data BindingForm = VariableDef | TypeDef
                 deriving (Show, Eq)

extendContext :: Context -> String -> BindingForm -> Ast -> Ty -> Context
extendContext ctx name form value ty = (VarBind name form value ty) : ctx

getTypeFromContext :: Context -> String -> Either String Ty
getTypeFromContext ctx name =
  case find byName ctx of Just (VarBind _ VariableDef _ ty) -> Right ty
                          _ -> Left $ "No type recorded for variable " ++ show name
  where byName (VarBind n VariableDef _ _) = n == name
        byName _ = False

getValueFromContext :: Context -> String -> Either String Ast
getValueFromContext ctx name =
  case find byName ctx of Just (VarBind _ VariableDef value _) -> Right value
                          _ -> Left $ "Unknown variable: " ++ show name
  where byName (VarBind n VariableDef _ _) = n == name
        byName _ = False
    
