module Interpreter
       (removeNames,
        eval)
       where

import Ast
import Control.Applicative
import Data.List (elemIndex)

type Store = [Ast]

-- Interpreter
extendstore :: Store -> Ast -> (Int, Store)
extendstore store t = (length store, store ++ [t])

updatestore :: Store -> Int -> Ast -> Store
updatestore (v':rest) 0 v = v':rest
updatestore (v':rest) n v = v:(updatestore rest (n-1) v)

eval :: Store -> Ast -> (Ast, Store)
eval store f@(Function _ _) = Closure f [1] 
eval store (TmApp t1 t2) = let ((TmAbs _ body), store') = eval store t1
                               (argu, store'')   = eval store' t2
                           in eval store'' $ termSubstTop argu body
eval store (TmRecord rs) = let (rs', store') = help rs store
                           in (TmRecord rs', store')
  where help [] s = ([], s)
        help ((l, t) : rest) s = let (t', st') = eval s t
                                     (rest', st'') = help rest st'
                                 in ((l, t'):rest', st'')

eval store (TmAccess tr s) = let ((TmRecord t), store') = eval store tr
                             in lookup s t store'
  where lookup s ((l, t1):ts)  st = if s == l then eval st t1 else lookup s ts st
eval store (TmIf (TmBool True) t2 t3) = eval store t2
eval store (TmIf (TmBool False) t2 t3) = eval store t3
eval store (TmIf t1 t2 t3) = let (t1', store') = eval store t1
                             in eval store' $ TmIf t1' t2 t3
eval store (TmRef t) = let (t', store') = eval store t
                           (l, store'') = extendstore store' t'
                       in eval store'' $ TmLocation l
eval store (TmDeRef (TmLocation i)) = (store !! i, store)
eval store (TmDeRef t) = let ((TmLocation l), store') = eval store t
                         in (store' !! l, store')
eval store (TmAssign t1 t2) = let ((TmLocation l), store') = eval store t1
                                  (t2', store'') = eval store' t2
                              in (TmUnit, updatestore store'' l t2')
eval store (TmBinary op t1 t2) =
  let ((TmNat t1'), store') = eval store t1
      ((TmNat t2'), store'') = eval store' t2
      numOp op = (TmNat (op t1' t2'), store'')
      logOp op = (TmBool (op t1' t2'), store'')
  in case op of "+" -> numOp (+)
                "-" -> numOp (-)
                "*" -> numOp (*)
                "/" -> numOp (div)
                ">" -> logOp (>)
                ">=" -> logOp (>=)
                "<" -> logOp (<)
                "<=" -> logOp (<=)
                "==" -> logOp (==)
                "!=" -> logOp (/=)
             
eval store t = (t, store)
