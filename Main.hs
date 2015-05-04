module Main where

import System.Environment
import System.IO
import Control.Applicative
import Control.Monad.Trans.Reader

import qualified Data.Map as Map

import Parser
--import TypeChecker
import Interpreter
import Ast

main :: IO ()
main = withFile
       "test/lambda.js"
       ReadMode
       (\handle -> do
           source <- hGetContents handle
           -- putStrLn $ show $ jsparse source
           mapM_ putStrLn $ interpreter source
       )
                                                 
-- Some glue code
interpreter :: String -> [String]
interpreter source = case jsparse source of Right ts -> map interp ts
                                            Left error -> [show error]
                                                              
  where interp t = show $ runReader (eval t) Map.empty

