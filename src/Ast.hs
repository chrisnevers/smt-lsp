module Ast where

import           Data.SExpresso.Parse
import           Data.SExpresso.SExpr

type LU = Located ()

type LS = Located String

type SE = SExpr LU LS

pretty :: SE -> String
pretty se = case se of
  SList lo ses    -> parens $ unwords $ map pretty ses
  SAtom (At lo s) -> s
  where
    parens s = "(" <> s <> ")"
