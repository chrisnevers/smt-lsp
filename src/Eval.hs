module Eval (eval, EBindings(..), ETypes(..), ERefs(..)) where


import           Ast
import           Control.Monad.Reader
import           Data.IORef
import qualified Data.Map             as M
import           Data.SExpresso.Parse
import           Data.SExpresso.SExpr

-- Every value is a function
type Type = ([String], String)

type EBindings = M.Map String [Location]

type ETypes = M.Map String Type

type ERefs = M.Map String [(String, Location)]

data Env = Env {
  e_bindings :: IORef EBindings,
  e_types    :: IORef ETypes,
  e_refs     :: IORef ERefs
}

type App = ReaderT Env IO

locOf :: SE -> Location
locOf se = case se of
  SList (At loc _) _ -> loc
  SAtom (At loc _)   -> loc

ps :: String -> App ()
ps = liftIO . putStrLn

addBinding :: String -> Location -> App ()
addBinding name loc = do
  e_bindings <- asks e_bindings
  liftIO $ modifyIORef e_bindings $ M.insertWith (flip (<>)) name [loc]

addType :: String -> Type -> App ()
addType name typeInfo = do
  e_types <- asks e_types
  liftIO $ modifyIORef e_types $ M.insert name typeInfo

declareFun :: Location -> [SE] -> App ()
declareFun loc = \case
  [name, SList _ f_args, ret] -> do
    let name' = pretty name
    addBinding name' loc
    addType name' (map pretty f_args, pretty ret)
  ow -> ps $ "declareFun: did not receive [name, args, ret]: " <> pretty ow

addReference :: String -> (String, Location) -> App ()
addReference name info = do
  e_refs <- asks e_refs
  liftIO $ modifyIORef e_refs $ M.insertWith (flip (<>)) name [info]

markUsed :: Location -> SE -> SE -> App ()
markUsed loc og = \case
  SAtom (At aloc v) -> do
    -- `v` is referenced/constrained by:
    --    * `og` at `loc`
    addReference v (pretty og, loc)
  SList _ vs -> mapM_ (markUsed loc og) vs

doAssert :: Location -> SE -> [SE] -> App ()
doAssert loc og args = do
  mapM_ (markUsed loc og) args

evalSExpr :: SE -> App ()
evalSExpr se = case se of
  SList _ ( SAtom (At _ kwd) : args) ->
    case kwd of
      "declare-fun" -> declareFun loc args
      "assert"      -> doAssert loc se args
      _             -> return ()
  SList _ _ -> return ()
  SAtom _ -> return ()
  where
    loc = locOf se

eval :: [SE] -> IO (EBindings, ETypes, ERefs)
eval ses = do
  e_bindings <- newIORef mempty
  e_types <- newIORef mempty
  e_refs <- newIORef mempty
  let env = Env {..}
  flip runReaderT env $ do
    mapM_ evalSExpr ses
  bindings <- readIORef e_bindings
  types <- readIORef e_types
  refs <- readIORef e_refs
  return (bindings, types, refs)
