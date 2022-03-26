module Parser where

import           Ast
import qualified Data.Functor.Identity      as F
import           Data.SExpresso.Parse
import           Data.SExpresso.SExpr
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type P = ParsecT () String F.Identity

atom :: P String
atom = some $ alphaNumChar <|> specialChars
  where
    specialChars = oneOf ("-:_;<=>+*/" :: [Char])

atomParser :: SExprParser P LU LS
atomParser = withLocation $ plainSExprParser atom

sexp :: P [SE]
sexp = decode $ setSpace withComments atomParser
  where
    withComments = L.space space1 (L.skipLineComment ";") empty

parseFile :: FilePath -> IO [SE]
parseFile filename = do
  str <- readFile filename
  case parse sexp "" str of
    Left err  -> fail $ "Error:" <> show err
    Right ast -> return ast
