module Ast where

import qualified Data.Map as Map

data Ast  = Var String
          | Function String Ast
          | Appliction Ast Ast
          | IfExpr Ast Ast Ast
          | LetExpr [(String, Ast)] Ast
          | Letrec  [(String, Ast)] Ast
          | Bool Bool
          | String String
          | Unit
          | Number Int
          | BinaryExpr String Ast Ast
          | Closure Ast Context
          | List [Ast]
          deriving (Show, Eq)

data Ty = TyFun Ty Ty
        | TyBool
        | TyUnit
        | TyNum
        | TyList Ty 
        deriving (Eq)

instance Show Ty where
  show (TyFun t1@(TyFun _ _) t2) = " (" ++ show t1 ++ ") ->" ++ show t2
  show (TyFun t1 t2) = show t1 ++ " ->" ++ show t2
  show TyBool = " Bool"
  show TyNum = " Number"
  show TyUnit = " Unit"
  show (TyList t) = " [" ++ show t ++ "]"

type Context = Map.Map String Ast
