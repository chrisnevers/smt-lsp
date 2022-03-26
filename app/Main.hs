module Main where

import Ast
import Eval
import Parser

main :: IO ()
main = do
  ast  <- parseFile filename
  putStrLn $ "The AST is:"
  mapM_ (putStrLn . pretty) ast
  (bindings, types) <- eval ast
  putStrLn $ "The binding environment is:"
  putStrLn $ show bindings
  putStrLn $ "The type environment is:"
  putStrLn $ show types
  where
    filename = "examples/index.main.smt"
