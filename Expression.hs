{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Expression where

import           Combinators
import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Char
import           Data.List
import           Text.Printf

data Operator = Pow
              | Mul
              | Div
              | Sum
              | Minus
              | Eq
              | Neq
              | Le
              | Lt
              | Ge
              | Gt
              | Conj
              | Disj
              deriving Eq

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a = BinOp Operator (EAst a) (EAst a)
            | Primary a
        deriving (Eq)

-- Change the signature if necessary
parseExpression :: String -> Either [String] (EAst Integer)
parseExpression input = Control.Arrow.left nub $ do
  (rest, eAST) <- runParser (spaced s) input
  when (rest /= "") $ Left ["remaining input: " ++ rest]
  return eAST
 where
  s  = b0 <|> n0
  b0 = binOp Disj b1 b0 <|> b1
  b1 = binOp Conj b2 b1 <|> b2
  b2 =
    binOp Eq n0 n0
      <|> binOp Neq n0 n0
      <|> binOp Le  n0 n0
      <|> binOp Lt  n0 n0
      <|> binOp Gt  n0 n0
      <|> binOp Ge  n0 n0
      <|> bracketed b0
  n0 = leftAssociativeBinOps [Sum, Minus] n1
  n1 = leftAssociativeBinOps [Mul, Div] n2
  n2 = binOp Pow n3 n2 <|> n3
  n3 =
    bracketed n0
      <|> Primary
      .   read
      <$> withErrorMessage ["expected number"] (   accept "0"
          <|> (:)
          <$> foldr ((<|>) . char) (failure []) "123456789"
          <*> many (foldr ((<|>) . char) (failure []) "0123456789")
          )
  binOp operator left right =
    BinOp operator <$> left <* spaced (accept (show operator)) <*> right
  leftAssociativeBinOps operators value = foldl (flip id) <$> value <*> many
    (foldr
      ( (<|>)
      . (\operator ->
          (flip $ BinOp operator)
            <$> (spaced (accept (show operator)) *> value)
        )
      )
      (failure [])
      operators
    )
  bracketed value = token '(' *> value <* token ')'
  spaced value = many space *> value <* many space
  space = Parser $ \case
    symbol : rest | isSpace symbol -> Right (rest, symbol)
    _                              -> Left []

instance Show Operator where
  show Pow   = "^"
  show Mul   = "*"
  show Div   = "/"
  show Sum   = "+"
  show Minus = "-"
  show Eq    = "=="
  show Neq   = "/="
  show Le    = "<="
  show Lt    = "<"
  show Ge    = ">="
  show Gt    = ">"
  show Conj  = "&&"
  show Disj  = "||"

instance Show a => Show (EAst a) where
  show = show' 0
   where
    show' n t =
      (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
          BinOp op l r -> printf "%s\n%s\n%s"
                                 (show op)
                                 (show' (ident n) l)
                                 (show' (ident n) r)
          Primary x -> show x
        )
    ident = (+ 1)

{-
show (BinOp Conj (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3))) (Primary 4))

&&
|_^
| |_1
| |_+
| | |_2
| | |_3
|_4
-}
