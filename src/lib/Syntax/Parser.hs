{-# LANGUAGE LambdaCase #-}
module Syntax.Parser where

import           Syntax.Ast

import           Bound
import           Control.Applicative
import           Data.Attoparsec.Text as Attoparsec
import           Data.Char
import           Data.List
import           Data.Monoid
import           Data.String
import qualified Data.Text            as Text

par :: String -> Either String (Exp Int Identifier)
par str = parseOnly (expression Nothing) (fromString str)

ser :: Exp Int Identifier -> String
ser = ser' 0
  where
    ser' n = \case
      Lam ls e -> "(\\" <> intercalate " " (map (\i -> Text.unpack (ls !! i) <> show (n + i)) [0 .. length ls - 1]) <> ". " <> ser' (n + length ls) (instantiate (\i -> Var $ ls !! i <> (Text.pack $ show (n + i))) e) <> ")"
      Var x -> Text.unpack x
      App a b -> "(" <> ser' n a <> " " <> ser' n b <> ")"
      Lit num -> show num


identifier :: Parser Identifier
identifier = Text.pack <$> do
  liftA2 (:) (satisfy firstChar) $ many (satisfy consequentChar)
  where
    firstChar = liftA2 (||) isAlpha (`elem` "_#")
    consequentChar = or <$> sequence [firstChar, isNumber, (`elem` "-")]

declaration :: Parser Declaration
declaration = do
  ident <- identifier
  skipSpace
  _ <- char '='
  skipSpace
  expr <- expression Nothing
  skipSpace <* char ';'
  return $ FunctionDeclaration ident expr

toplevel :: Parser [Declaration]
toplevel = skipSpace *> many (declaration <* skipSpace) <* endOfInput

inParen :: Parser a -> Parser a
inParen p = char '(' *> p <* char ')'

literal :: Parser Int
literal = signed decimal

expression :: Maybe (Exp Int Identifier) -> Parser (Exp Int Identifier)
expression lhs
  = choice
    [ do
      expr <- char '(' *> skipSpace *> expression Nothing <* skipSpace <* char ')'
      skipSpace
      expression $ Just (appOrNot expr)
    , do
      idents <- char '\\' *> skipSpace *> many1 (identifier <* skipSpace) <* string (fromString "->")
      skipSpace
      expr <- expression Nothing
      return . appOrNot $ Lam idents $ abstract (\i -> elemIndex i idents) expr
    , do
      ident <- identifier
      skipSpace
      expression $ Just (appOrNot (Var ident))
    , do
      lit <- literal
      skipSpace
      expression $ Just (appOrNot (Lit lit))
    , case lhs of
         Nothing -> empty
         Just l -> return l
    ]
  where
    appOrNot e = case lhs of
      Nothing -> e
      Just l -> App l e
