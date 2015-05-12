module Interpreter
       (interpreate)
       where

import Ast
import Control.Applicative
import Control.Monad.Trans.Reader
import Data.Maybe
import qualified Data.Map as Map

type Environment = Reader Context
             
-- Interpreter
interpreate :: Ast -> Ast
interpreate t = runReader (eval t) Map.empty
    
getContext :: Environment Context
getContext = ask

eval :: Ast -> Environment Ast
eval (Var x) = do env <- getContext
                  return $ fromJust $ Map.lookup x env
                  
eval f@(Function _ _) = do env <- getContext
                           return $ Closure f env

eval (Appliction t1 t2) = do (Closure (Function var body) env') <- eval t1
                             argu <- eval t2
                             local (const (Map.insert var argu env')) (eval body)

eval (LetExpr binds body) = do env <- getContext
                               let newBinds = map (\(var, term) ->
                                                    (var, runReader (eval term) env)) binds
                               local (const
                                      (Map.union (Map.fromList newBinds) env)) (eval body)

eval (LetRec [] body) = eval body
eval (LetRec binds body) = getContext >>= \env ->
  let newEnv = Map.union (Map.fromList newBinds) env
      newBinds = map (\(var, term) -> (var, runReader (eval term) newEnv)) binds
  in local (const newEnv) (eval body)
                             
eval (IfExpr (Bool True) t2 _) = eval t2
eval (IfExpr (Bool False) _ t3) = eval t3
eval (IfExpr t1 t2 t3) = do t1' <- eval t1
                            eval $ IfExpr t1' t2 t3

eval (List l) = List <$> mapM eval l
eval (Car l) = do List l' <- eval l
                  return $ head l'
eval (Cdr l) = do List l' <- eval l
                  return $ List $ tail l'
eval (IsNil l) = do List l' <- eval l
                    return $ Bool $ null l'
eval (Cons h l) = do h' <- eval h
                     List l' <- eval l
                     return $ List $ h' : l'

eval (BinaryExpr op t1 t2) =
  do Number t1' <- eval t1
     Number t2' <- eval t2
     let numOp operator = Number (operator t1' t2')
     let logOp operator = Bool (operator t1' t2')
     return $ case op of "+" -> numOp (+)
                         "-" -> numOp (-)
                         "*" -> numOp (*)
                         "/" -> numOp div
                         ">" -> logOp (>)
                         ">=" -> logOp (>=)
                         "<" -> logOp (<)
                         "<=" -> logOp (<=)
                         "==" -> logOp (==)
                         "!=" -> logOp (/=)
                         _ -> error "Arithmatic error"
                         
eval t = return t


{-
extendstore' :: Store -> Ast -> (Int, Store)
extendstore' store t = (length store, store ++ [t])

updatestore' :: Int -> Ast -> Store-> Store
updatestore' 0 v (v':rest) = v':rest
updatestore' n v (v':rest) = v : updatestore' (n-1) v rest

updatestore :: Int -> Ast -> Environment ()
updatestore location newValue =  modify $ updatestore' location newValue

extendstore :: Ast -> Environment Int
extendstore t = do s <- get
                   let (newLocation, newStore) = extendstore' s t
                   put newStore
                   return newLocation
-}
