module Eval where


import           Ast
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Map             as M
import           Data.SExpresso.Parse
import           Data.SExpresso.SExpr

-- Every value is a function
type Type = ([String], String)

data Env = Env {
  e_bindings :: IORef (M.Map String [Location]),
  e_types    :: IORef (M.Map String Type)
}

type App = ReaderT Env IO

locOf :: SE -> Location
locOf se = case se of
  SList (At loc _) _ -> loc
  SAtom (At loc _)   -> loc

log' :: String -> App ()
log' = liftIO . putStrLn

addBinding :: String -> Location -> App ()
addBinding name loc = do
  e_bindings <- asks e_bindings
  liftIO $ modifyIORef e_bindings $ M.insertWith (flip (<>)) name [loc]

addType :: String -> Type -> App ()
addType name typeInfo = do
  e_types <- asks e_types
  liftIO $ modifyIORef e_types $ M.insert name typeInfo

declareFun :: Location -> [SE] -> App ()
declareFun loc args = do
  case args of
    [name, SList _ f_args, ret] -> do
      let name' = pretty name
      addBinding name' loc
      addType name' (map pretty f_args, pretty ret)
    _ -> do
      log' $ "declareFun: did not receive [name, args, ret]: " <> (unwords $ map pretty args)

evalSExpr :: SE -> App ()
evalSExpr se = case se of
  SList _ ( SAtom (At _ kwd) : args) ->
    case kwd of
      "declare-fun" -> declareFun loc args
      _             -> return ()
  SList _ _ -> return ()
  SAtom _ -> return ()
  where
    loc = locOf se

eval :: [SE] -> IO (M.Map String [Location], M.Map String Type)
eval ses = do
  e_bindings <- newIORef mempty
  e_types <- newIORef mempty
  let env = Env {..}
  flip runReaderT env $ do
    mapM_ evalSExpr ses
  bindings <- readIORef e_bindings
  types <- readIORef e_types
  return (bindings, types)
