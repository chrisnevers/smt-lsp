module Main where

import Ast
import Eval
import Parser
import qualified Data.Map as M

main :: IO ()
main = do
  ast  <- parseFile filename
  putStrLn $ "The AST is:"
  mapM_ (putStrLn . pretty) ast
  (bindings, types, refs) <- eval ast
  putStrLn $ "The binding environment is:"
  putStrLn $ pretty bindings
  putStrLn $ "The type environment is:"
  putStrLn $ pretty types
  putStrLn $ "The refs environment is:"
  putStrLn $ pretty $ M.map (\ ss -> map (\ (se, at) -> se <> " @ " <> pretty at) ss) refs
  where
    filename = "examples/index.main.smt"
