module Main where

import System.Environment
import System.IO
import Control.Applicative
import Control.Monad

import Parser
import TypeChecker
--import Interpreter
import Ast

main :: IO ()
main = do withFile
            "test/lambda.js"
            ReadMode
            (\handle -> do
                source <- hGetContents handle
                -- putStrLn $ show $ jsparse source
                mapM_ putStrLn $ interpreter source
            )
                                                 
-- Some glue code
interpreter :: String -> [String]
interpreter source = case jsparse source of Right ts -> map takeOut (map interp ts)
                                            Left error -> [show error]
                                                              
  where interp t = do ty <- typeOf [] t
                      return $ show ty
                      --return $ show (fst $ eval [] dt) ++ ": " ++ show ty
        takeOut (Right str) = str
        takeOut (Left str)  = str
