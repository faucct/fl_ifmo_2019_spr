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
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           Text.Printf

data BinOperator = Pow
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

relation operator a b = if operator a b then 1 else 0

booleanOperator operator = relation (operator `on` ((/= 0) . fromInteger))

binOperatorConstructor :: BinOperator -> Integer -> Integer -> Integer
binOperatorConstructor Pow   = (^)
binOperatorConstructor Mul   = (*)
binOperatorConstructor Div   = div
binOperatorConstructor Sum   = (+)
binOperatorConstructor Minus = (-)
binOperatorConstructor Eq    = relation (==)
binOperatorConstructor Neq   = relation (/=)
binOperatorConstructor Le    = relation (<=)
binOperatorConstructor Lt    = relation (<)
binOperatorConstructor Ge    = relation (>=)
binOperatorConstructor Gt    = relation (>)
binOperatorConstructor Conj  = booleanOperator (&&)
binOperatorConstructor Disj  = booleanOperator (||)

data UnOperator = Neg | Not deriving Eq

unOperatorConstructor :: UnOperator -> Integer -> Integer
unOperatorConstructor Neg = negate
unOperatorConstructor Not = \value -> if value == 0 then 1 else 0

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a identifier = BinOp BinOperator (EAst a identifier) (EAst a identifier)
            | UnOp UnOperator (EAst a identifier)
            | Primary a
            | Identifier identifier
        deriving (Eq)

bracketed
  :: (Applicative err, TokenContainer tokenContainer Char)
  => Parser [tokenContainer] (err String) a
  -> Parser [tokenContainer] (err String) a
bracketed parser = char '(' *> parser <* char ')'

-- Change the signature if necessary
-- Constructs AST for the input expression
parseExpression :: String -> Either [String] (EAst Integer String)
parseExpression =
  let space = Parser $ \case
        symbol : rest | isSpace symbol -> Right (rest, symbol)
        _                              -> Left empty
      primaryParser =
          Primary
            .   read
            <$> (   accept "0"
                <|> (:)
                <$> foldr ((<|>) . char) (failure []) "123456789"
                <*> many (foldr ((<|>) . char) (failure []) "0123456789")
                )
      unOpParser =
          UnOp
            <$> foldr
                  (\unOperator ->
                    (accept (show unOperator) *> success unOperator <|>)
                  )
                  empty
                  [Neg, Not]
            <* many space
            <*> (primaryParser <|> identifierParser <|> bracketed expressionParser
                )
      identifierParser =
          curry (Identifier . uncurry (:))
            <$> (Parser $ \case
                  (char : rest) | isLower char || char == '_' ->
                    Right (rest, char)
                  _ -> Left ["expected a lower character or underscore"]
                )
            <*> many
                  (Parser $ \case
                    (char : rest) | isAlphaNum char -> Right (rest, char)
                    _                               -> Left []
                  )
      expressionParser = expression
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
        (   bracketed expressionParser
        <|> primaryParser
        <|> identifierParser
        <|> unOpParser
        )
  in  runParserUntilEof expressionParser

optimizeExpression :: EAst Integer String -> EAst Integer String
optimizeExpression primary@(   Primary    _) = primary
optimizeExpression identifier@(Identifier _) = identifier
optimizeExpression (BinOp op left right) =
  let
    optimizedLeft  = optimizeExpression left
    optimizedRight = optimizeExpression right
  in
    case op of
      Sum | optimizedLeft == Primary 0  -> optimizedRight
      Sum | optimizedRight == Primary 0 -> optimizedLeft
      Mul | optimizedLeft == Primary 0 || optimizedRight == Primary 0 ->
        Primary 0
      Mul | optimizedLeft == Primary 1  -> optimizedRight
      Mul | optimizedRight == Primary 1 -> optimizedLeft
      _
        | Primary leftPrimary <- optimizedLeft, Primary rightPrimary <-
          optimizedRight
        -> Primary $ binOperatorConstructor op leftPrimary rightPrimary
      _ -> BinOp op optimizedLeft optimizedRight
optimizeExpression (UnOp operator value) =
  let optimizedValue = optimizeExpression value
  in  case optimizedValue of
        Primary primaryValue ->
          Primary $ unOperatorConstructor operator primaryValue
        _ -> UnOp operator optimizedValue

-- Change the signature if necessary
-- Calculates the value of the input expression
executeExpression :: String -> Either [String] (Map String Integer -> Integer)
executeExpression =
  let
    space = Parser $ \case
      symbol : rest | isSpace symbol -> Right (rest, symbol)
      _                              -> Left empty
    primaryParser =
      const
        <$> (   read
            <$> (   accept "0"
                <|> (:)
                <$> foldr ((<|>) . char) (failure []) "123456789"
                <*> many (foldr ((<|>) . char) (failure []) "0123456789")
                )
            )
    unOpParser =
      foldr
          (\(unOperator, constructor) ->
            (accept (show unOperator) *> success (constructor .) <|>)
          )
          empty
          [ (Neg, negate)
          , ( Not
            , \case
              0 -> 1
              _ -> 0
            )
          ]
        <*  many space
        <*> (primaryParser <|> identifierParser <|> bracketed expressionParser
        )
    identifierParser =
      curry (flip (Map.!) . uncurry (:))
        <$> (Parser $ \case
              (char : rest) | isLower char || char == '_' -> Right (rest, char)
              _ -> Left ["expected a lower character or underscore"]
            )
        <*> many
              (Parser $ \case
                (char : rest) | isAlphaNum char -> Right (rest, char)
                _                               -> Left []
              )
    expressionParser = expression
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
      , ( LAssoc
        , [(accept "+", primaryOperator (+)), (accept "-", primaryOperator (-))]
        )
      , ( LAssoc
        , [(accept "*", primaryOperator (*)), (accept "/", primaryOperator div)]
        )
      , (RAssoc, [(accept "^", primaryOperator (^))])
      ]
      (   bracketed expressionParser
      <|> primaryParser
      <|> unOpParser
      <|> identifierParser
      )
  in
    runParserUntilEof expressionParser
 where
  relation operator a b context =
    if (operator `on` ($ context)) a b then 1 else 0
  booleanOperator
    :: (Bool -> Bool -> Bool)
    -> (Map String Integer -> Integer)
    -> (Map String Integer -> Integer)
    -> Map String Integer
    -> Integer
  booleanOperator operator = relation (operator `on` ((/= 0) . fromInteger))
  primaryOperator operator left right context =
    (operator `on` ($ context)) left right

instance Show BinOperator where
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

instance Show UnOperator where
  show Neg = "-"
  show Not = "!"

instance (Show a, Show identifier) => Show (EAst a identifier) where
  show = show' 0
   where
    show' n t =
      (if n > 0 then printf "%s|_%s" (concat (replicate (n - 1) "| ")) else id)
        (case t of
          BinOp op l r -> printf "%s\n%s\n%s"
                                 (show op)
                                 (show' (ident n) l)
                                 (show' (ident n) r)
          UnOp op v             -> printf "%s\n%s" (show op) (show' (ident n) v)
          Primary    x          -> show x
          Identifier identifier -> show identifier
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
