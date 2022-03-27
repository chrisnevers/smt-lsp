module Ast where

import           Data.List            (intercalate)
import qualified Data.Map             as M
import           Data.SExpresso.Parse
import           Data.SExpresso.SExpr
import           Text.Megaparsec

type LU = Located ()

type LS = Located String

type SE = SExpr LU LS

class Pretty a where
  pretty :: a -> String

instance (Pretty a, Pretty b) => Pretty (a, b) where
  pretty (a, b) = "(" <> pretty a <> ", " <> pretty b <> ")"

instance Pretty SE where
  pretty = \case
    SList lo ses    -> parens $ unwords $ map pretty ses
    SAtom (At lo s) -> s
    where
      parens s = "(" <> s <> ")"

instance Pretty a => Pretty [a] where
  pretty ss = "[" <> intercalate ", " (map pretty ss) <> "]"

instance {-# OVERLAPPING #-} Pretty [SE] where
  pretty ss = "(" <> unwords (map pretty ss) <> ")"

instance {-# OVERLAPPING #-} Pretty String where
  pretty = id

instance Pretty Pos where
  pretty p = show $ unPos p

instance Pretty SourcePos where
  pretty (SourcePos {..}) = sourceName <> ":" <> pretty sourceLine <> ":" <> pretty sourceColumn

instance Pretty Location where
  pretty = \case Span sp sp' -> pretty sp

instance Pretty a => Pretty (Located a) where
  pretty (At start _) = pretty start

instance (Pretty a, Pretty b) => Pretty (M.Map a b) where
  pretty m =
    unlines $ map (\ (k, v) -> pretty k <> " : " <> pretty v) $ M.toAscList m
