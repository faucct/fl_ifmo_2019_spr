{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
module Expression where

import           Text.Printf
import           Combinators
import           Combinators
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Function
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
-- Constructs AST for the input expression
parseExpression :: String -> Either [String] (EAst Integer)
parseExpression = runParserUntilEof
  (expression
    [ (RAssoc, [(accept "||", BinOp Disj)])
    , (RAssoc, [(accept "&&", BinOp Conj)])
    , ( NAssoc
      , [ (accept "==", BinOp Eq)
        , (accept "/=", BinOp Neq)
        , (accept "<=", BinOp Le)
        , (accept "<" , BinOp Lt)
        , (accept ">=", BinOp Ge)
        , (accept ">" , BinOp Gt)
        , (accept "-" , BinOp Minus)
        ]
      )
    , (LAssoc, [(accept "+", BinOp Sum), (accept "-", BinOp Minus)])
    , (LAssoc, [(accept "*", BinOp Mul), (accept "/", BinOp Div)])
    , (RAssoc, [(accept "^", BinOp Pow)])
    ]
    (   Primary
    .   read
    <$> (   accept "0"
        <|> (:)
        <$> foldr ((<|>) . char) (failure []) "123456789"
        <*> many (foldr ((<|>) . char) (failure []) "0123456789")
        )
    )
  )

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either [String] Integer
executeExpression = runParserUntilEof
  (expression
    [ (RAssoc, [(accept "||", booleanOperator (||))])
    , (RAssoc, [(accept "&&", booleanOperator (&&))])
    , ( NAssoc
      , [ (accept "==", relation (==))
        , (accept "/=", relation (/=))
        , (accept "<=", relation (<=))
        , (accept "<" , relation (<))
        , (accept ">=", relation (>=))
        , (accept ">" , relation (>))
        ]
      )
    , (LAssoc, [(accept "+", (+)), (accept "-", (-))])
    , (LAssoc, [(accept "*", (*)), (accept "/", div)])
    , (RAssoc, [(accept "^", (^))])
    ]
    (   read
    <$> (   accept "0"
        <|> (:)
        <$> foldr ((<|>) . char) (failure []) "123456789"
        <*> many (foldr ((<|>) . char) (failure []) "0123456789")
        )
    )
  )
 where
  relation operator a b = if operator a b then 1 else 0
  booleanOperator = (`on` (/= 0) . fromInteger) . relation

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
