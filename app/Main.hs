{-# OPTIONS_GHC -Wno-missing-fields #-}
module Main where

import           Ast
import           Control.Exception             (Exception, try)
import           Control.Lens                  ((^.))
import           Control.Monad.Reader
import           Data.IORef
import           Data.List                     (intercalate)
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.SExpresso.Parse.Location as S
import           Data.String                   (IsString)
import qualified Data.Text                     as T
import           Eval
import           Language.LSP.Diagnostics
import           Language.LSP.Server
import           Language.LSP.Types            hiding (length)
import qualified Language.LSP.Types.Lens       as LSPLens
import           Parser
import           Safe                          (atMay)
import           System.Log.Logger
import           Text.Megaparsec               (SourcePos (SourcePos, sourceColumn, sourceLine),
                                                unPos)

-- cliMain :: IO ()
-- cliMain = do
--   ast  <- parseFile filename
--   putStrLn $ "The AST is:"
--   mapM_ (putStrLn . pretty) ast
--   (bindings, types, refs) <- eval ast
--   putStrLn $ "The binding environment is:"
--   putStrLn $ pretty bindings
--   putStrLn $ "The type environment is:"
--   putStrLn $ pretty types
--   putStrLn $ "The refs environment is:"
--   putStrLn $ pretty $ M.map (\ ss -> map (\ (se, at) -> se <> " @ " <> pretty at) ss) refs
--   where
--     filename = "examples/index.main.smt"

type Env = (EBindings, ETypes, ERefs)

data LspEnv = LspEnv
  { e_eval_env  :: IORef Env,
    e_compiling :: IORef Bool }

type SMTExcept = String

instance Exception SMTExcept

type CompileResult = Either SMTExcept Env

type App = ReaderT LspEnv (LspM ())


logger :: MonadIO m => String -> m ()
logger msg = do
  liftIO $ emergencyM "smtide" msg


getFilePath :: LSPLens.HasUri s Uri => s -> Maybe FilePath
getFilePath = uriToFilePath . grabUri


grabUri :: LSPLens.HasUri s Uri => s -> Uri
grabUri doc = doc ^. LSPLens.uri


srcToPos :: SourcePos -> Position
srcToPos = \case
  SourcePos {..} ->
    let line = toEnum $ unPos sourceLine in
    let col  = toEnum $ unPos sourceColumn in
    Position (line - 1) (col - 1)


locToPos :: S.Location -> (Position, Position)
locToPos (S.Span sp ep) = (srcToPos sp, srcToPos ep)


maxDiags :: Int
maxDiags = 100


clearDiags :: MonadLsp config m => DiagnosticSource -> m ()
clearDiags uri = flushDiagnosticsBySource 0 $ Just uri


makeErrorDiag :: Position -> Position -> String -> Diagnostic
makeErrorDiag start end msg =
  Diagnostic (Range start end) (Just DsError) Nothing (Just "reachide") (T.pack msg) Nothing (Just $ List [])


publishDiags :: NormalizedUri -> [Diagnostic] -> App ()
publishDiags nUri diags = publishDiagnostics maxDiags nUri (Just 0) $ partitionBySource diags


onInitialized :: (MonadIO m, Show a) => a -> m ()
onInitialized _ = do
  logger "Initialized SMT LSP"


getClosestWord :: Maybe FilePath -> Int -> Int -> App (Maybe String)
getClosestWord (Just path) lineNum column = do
  ls <- lines <$> liftIO (readFile path)
  let line = atMay ls lineNum
  let aux :: Int -> String -> App (Maybe String)
      aux i cs = do
        let spacesLen = Prelude.length $ takeWhile (== ' ') cs
        let w = takeWhile (/= ' ') $ dropWhile (== ' ') cs
        let wl = Prelude.length w + spacesLen
        case wl + i > column of
          True  -> return (Just w)
          False -> aux (wl + i) $ drop wl cs
  word <- maybe (return Nothing) (aux 0) line
  return word
  where
getClosestWord Nothing _ _ = do
  logger "getClosestWord: No filepath"
  return Nothing


mdBlock :: (Semigroup a, IsString a) => a -> a
mdBlock s = "\n```smt-lib\n" <> s <> "\n```\n"


prettyType :: ([String], String) -> String
prettyType = \case
  ([], ret)   -> pretty ret
  (args, ret) -> intercalate " -> " (map pretty args) <> " -> " <> pretty ret


mdLink :: (Semigroup a, IsString a) => a -> a -> a
mdLink lab link = "[" <> lab <> "](" <> link <> ")"


mdLocalLink :: (Semigroup a, IsString a) => a -> a
mdLocalLink lab = "[" <> lab <> "](" <> "vscode://file/" <> lab <> ")"


prettyDefine :: Pretty a => a -> String
prettyDefine b = "* " <> mdLocalLink (pretty b) <> "\n"


prettyDefines :: Pretty a => [a] -> String
prettyDefines bs =
  "#### Definitions\n" <>
    unlines (map prettyDefine bs)


prettyRef :: (Pretty a1, Pretty a2) => (a1, a2) -> String
prettyRef (se, loc) = mdBlock (pretty se) <> "at: " <> mdLocalLink (pretty loc) <> "\n"


prettyRefs :: (Pretty a1, Pretty a2) => [(a1, a2)] -> String
prettyRefs rs =
  "#### References\n" <>
    unlines (map (prettyRef) rs)


hsep :: String
hsep = "\n---\n\n"


typeAnnot :: String -> ([String], String) -> String
typeAnnot v t = v <> ": " <> prettyType t


getInfo :: String -> App (Maybe String, Maybe String, Maybe String)
getInfo v = do
  envRef <- asks e_eval_env
  (bindings, types, refs) <- liftIO $ readIORef envRef
  let bi = maybe Nothing (Just . prettyDefines)         $ M.lookup v bindings
  let ti = maybe Nothing (Just . mdBlock . typeAnnot v) $ M.lookup v types
  let ri = maybe Nothing (Just . prettyRefs)            $ M.lookup v refs
  return (ti, bi, ri)


-- Prefix with "```\n" to automatically close the block thats inserted
md :: String -> MarkupContent
md s = markedUpContent "markdown" $ T.pack ("```\n" <> s)


onDocumentHover :: Handler App 'TextDocumentHover
onDocumentHover req responder = do
  logger $ "onDocumentHover"
  let RequestMessage _ _ _ (HoverParams doc pos _) = req
  let range = Range pos pos
  let path = getFilePath doc
  ident <- fromMaybe "" <$> getClosestWord path (fromEnum $ _line pos) (fromEnum $ _character pos)
  (ti, bi, ri) <- getInfo ident
  let info = catMaybes [ti, bi, ri]
  let msg = HoverContents $ md $ intercalate hsep info
  let rsp = Hover msg $ Just range
  responder $ Right $ if info == [] then Nothing else Just rsp


onSignatureHelp :: Handler App 'TextDocumentSignatureHelp
onSignatureHelp req responder = do
  logger $ "onSignatureHelp"
  let RequestMessage _ _ _ (SignatureHelpParams doc pos _ _) = req
  let range = Range pos pos
  let path = getFilePath doc
  ident <- fromMaybe "" <$> getClosestWord path (fromEnum $ _line pos) (fromEnum $ _character pos)
  (ti, _, _) <- getInfo ident
  let sis = case ti of
        Nothing -> List $ []
        Just ty -> do
          let mu = SignatureHelpDocMarkup $ md ty
          List $ [SignatureInformation (T.pack ident) (Just mu) Nothing Nothing]
  let rsp = SignatureHelp sis Nothing Nothing
  responder $ Right $ rsp


doCompile :: Uri -> App ()
doCompile uri = do
  logger "doCompile"
  e_compiling <- asks e_compiling
  isCompiling <- liftIO $ readIORef e_compiling
  case (mPath, isCompiling) of
    (Just filepath, False) -> do
      ast <- liftIO $ parseFile filepath
      (e :: CompileResult) <- liftIO $ try $ eval ast
      liftIO $ modifyIORef e_compiling $ return False
      clearDiags uriT
      case e of
        Left msg -> do
          -- XXX Fix me
          let pos   = Position 0 0
          let diags = [makeErrorDiag pos pos msg]
          logger $ "Error during compilation: " <> msg
          publishDiags nUri diags
        Right menv -> do
          logger "Compiled successfully"
          evalEnv <- asks e_eval_env
          liftIO $ writeIORef evalEnv $ menv
    (_, _) -> return ()
  where
    uriT  = getUri uri
    nUri  = toNormalizedUri uri
    mPath = uriToFilePath uri


onDocumentChange :: Handler App 'TextDocumentDidChange
onDocumentChange msg = do
  logger "onDocumentChange"
  let uri = grabUri $ msg ^. LSPLens.params . LSPLens.textDocument
  doCompile uri


onDocumentOpen :: Handler App 'TextDocumentDidOpen
onDocumentOpen msg = do
  logger "onDocumentOpen"
  let uri = grabUri $ msg ^. LSPLens.params . LSPLens.textDocument
  doCompile uri


onWorkspaceDidChange :: Handler App 'WorkspaceDidChangeWatchedFiles
onWorkspaceDidChange msg = do
  logger "onWorkspaceDidChange"
  let uris = fmap grabUri $ msg ^. LSPLens.params . LSPLens.changes
  mapM_ doCompile uris


onDocumentCompletion :: Handler App 'TextDocumentCompletion
onDocumentCompletion _req responder = do
  let l = CompletionList False (List [])
  responder $ Right $ InR l


handlers :: Handlers App
handlers = mconcat
  [ notificationHandler SInitialized onInitialized
  , requestHandler      STextDocumentHover onDocumentHover
  , requestHandler      STextDocumentSignatureHelp onSignatureHelp
  , notificationHandler STextDocumentDidChange onDocumentChange
  , notificationHandler STextDocumentDidOpen onDocumentOpen
  , notificationHandler SWorkspaceDidChangeWatchedFiles onWorkspaceDidChange
  , requestHandler      STextDocumentCompletion onDocumentCompletion ]


main :: IO Int
main = do
  e_eval_env <- liftIO $ newIORef (mempty, mempty, mempty)
  e_compiling <- liftIO $ newIORef False
  let appEnv = LspEnv {..}
  runServer $ ServerDefinition
    { onConfigurationChange = const $ pure $ Right ()
    , doInitialize = \ env _req -> pure $ Right env
    , staticHandlers = handlers
    , interpretHandler = \ env ->
        Iso (runLspT env . flip runReaderT appEnv) liftIO
    , options = defaultOptions { textDocumentSync = syncOptions }
    }
  where
    willSave = Nothing
    onOpenClose = Just True
    onChange = Just TdSyncIncremental
    willSaveWaitUntil = Nothing
    onSave = Just $ InL True
    syncOptions = Just $ TextDocumentSyncOptions onOpenClose onChange willSave willSaveWaitUntil onSave
