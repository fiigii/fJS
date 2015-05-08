module Main where

import System.IO
import Control.Applicative
import Control.Monad.Trans.Reader

import qualified Data.Map as Map

import Parser
import TypeInferencer
import Interpreter
import Ast

main :: IO ()
main = withFile
       "test/lambda.js"
       ReadMode
       (\handle -> do
           source <- hGetContents handle
           -- putStrLn $ show $ jsparse source
           putStrLn $ interpreter source
       )
                                                 
-- Some glue code
interpreter :: String -> String
interpreter source = case jsparse source of Right ts -> case interp ts of Right s -> s
                                                                          Left e -> e
                                            Left error -> show error
                        
                                                              
  where interp t = do ty <- infer t
                      return $ show (interpreate t) ++ " : " ++ show ty 

