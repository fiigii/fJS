module Ast where

import qualified Data.Map as Map
import Data.List (intersperse)

type Context = Map.Map String Ast

data Ast  = Var String
          | Function String Ast
          | Application Ast Ast
          | IfExpr Ast Ast Ast
          | LetExpr [(String, Ast)] Ast
          | LetRec  [(String, Ast)] Ast
          | Bool Bool
          | String String
          | Unit
          | Number Integer
          | BinaryExpr String Ast Ast
          | Closure Ast Context
          | List [Ast]
          | Car Ast
          | Cdr Ast
          | Cons Ast Ast
          | IsNil Ast
          deriving (Eq)

data Type = TyVar String
          | TyBool
          | TyUnit
          | TyNum
          | TyString
          | TyList Type
          | TyFun Type Type
            deriving (Eq)
                     
data Scheme = Forall [String] Type
              
comparer =  ["==", "!=", "<=", "<", ">=", ">"]
arith = ["+", "-", "*", "/"]

-- Print
instance Show Type where
  show (TyFun t1@(TyFun _ _) t2) = "(" ++ show t1 ++ ") -> " ++ show t2
  show (TyFun t1 t2) = show t1 ++ " -> " ++ show t2
  show (TyVar v) = v
  show TyBool = "Bool"
  show TyNum = "Number"
  show TyUnit = "Unit"
  show TyString = "String"
  show (TyList t) = "[" ++ show t ++ "]"

instance Show Ast where
  show (Var v) = v
  show (Closure _ _) = "func"
  show (Bool b) = show b
  show (String s) = s
  show Unit = "()"
  show (Number n) = show n
  show (List l) = "[" ++ unwords (intersperse "," $ map show l) ++ "]"
  show _ = ""

instance Show Scheme where
  show (Forall [] ty) = show ty
  show (Forall vs ty) = "forall " ++ unwords (intersperse "," vs) ++ ". " ++ show ty
