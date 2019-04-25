{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
module Expression where

import           Text.Printf
import           Combinators
import           Control.Applicative
import           Control.Monad
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.List
import           Combinators
import           Data.Functor.Compose
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

spaceParser :: Parser String [a] Char
spaceParser = Parser $ \case
  symbol : rest | isSpace symbol -> Right (rest, symbol)
  _                              -> Left empty

infixl 4 :@>
infixr 3 :->

newtype Fix f = Fix { unFix :: (f (Fix f))}

instance Eq (f (Fix f)) => Eq (Fix f) where
  (==) = (==) `on` unFix

data DataType value = DataType String (Map String [TypeReference value]) deriving (Show, Eq, Functor, Foldable, Traversable)
intDataType = Fix $ DataType "Int" Map.empty
boolDataType =
  Fix $ DataType "Bool" $ Map.fromList [("True", []), ("False", [])]
data TypeReference value = TypeReference value | TypeReference value :@> TypeReference value | TypeReference value :-> TypeReference value deriving (Show, Eq, Functor, Foldable, Traversable)
data TypeAlias value = TypeAlias String (TypeReference value) deriving Show

typeReferenceParser :: Parser String [String] (TypeReference String)
typeReferenceParser =
  TypeReference <$> typeIdentifierParser <|> bracketed headTypeReferenceParser

headTypeReferenceParser :: Parser String [String] (TypeReference String)
headTypeReferenceParser = expression
  [(LAssoc, [(some spaceParser, (:@>))])]
  (TypeReference <$> typeIdentifierParser <|> bracketed headTypeReferenceParser)

guardNotReserved identifier =
  when (identifier `elem` ["let", "data", "type", "in", "if", "then", "else"])
    $ failure [identifier ++ " is a reserved keyword"]

typeIdentifierParser :: Parser String [String] String
typeIdentifierParser = do
  initial <- headParser
  unless (isUpper initial) $ failure ["expected upper"]
  rest <- many $ do
    subsequent <- headParser
    guard $ isAlphaNum subsequent
    return subsequent
  let identifier = initial : rest
  guardNotReserved identifier
  return identifier

dataDefinitionParser :: Parser String [String] (DataType String)
dataDefinitionParser =
  let constructorParser = do
        constructor <- typeIdentifierParser
        arguments   <- many (many spaceParser *> typeReferenceParser)
        return (constructor, arguments)
  in
    do
      _              <- accept "data"
      _              <- some spaceParser
      typeIdentifier <- typeIdentifierParser
      _              <- many spaceParser
      constructors   <-
        (do
            _           <- accept "="
            _           <- many spaceParser
            constructor <- constructorParser
            rest        <-
              many
                (  many spaceParser
                *> accept "|"
                *> many spaceParser
                *> constructorParser
                )
            return $ constructor : rest
          )
          <|> success []
      return $ DataType typeIdentifier $ Map.fromList constructors

typeAliasParser :: Parser String [String] (TypeAlias String)
typeAliasParser = do
  _              <- accept "type"
  _              <- some spaceParser
  typeIdentifier <- typeIdentifierParser
  _              <- many spaceParser
  _              <- accept "="
  _              <- many spaceParser
  TypeAlias typeIdentifier <$> headTypeReferenceParser

type TypeSystem value = [Either (DataType value) (TypeAlias value)]
typeSystemParser :: Parser String [String] (TypeSystem String)
typeSystemParser =
  many spaceParser
    *> (   success []
       <*  eofParser
       <|> let parser =
                 Left <$> dataDefinitionParser <|> Right <$> typeAliasParser
           in  (:) <$> parser <* many spaceParser <*> manyUntil
                 eofParser
                 (accept ";" *> many spaceParser *> parser <* many spaceParser
                 )
       )

data Pattern identifier pattern = VariablePattern pattern | ConstructorPattern identifier [Pattern identifier pattern] deriving (Eq, Show)

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a identifier = BinOp BinOperator (EAst a identifier) (EAst a identifier)
            | UnOp UnOperator (EAst a identifier)
            | Primary a
            | Identifier identifier
            | Function (Pattern identifier (identifier, TypeReference String)) (EAst a identifier)
            | Application (EAst a identifier) (EAst a identifier)
            | Let (Pattern identifier identifier) (EAst a identifier) (EAst a identifier)
            | If (EAst a identifier) (EAst a identifier) (EAst a identifier)
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
  let
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
        <*  many spaceParser
        <*> expressionParser
    lIdentifierParser = do
      initial <- headParser
      unless (isLower initial || initial == '_')
        $ failure ["expected a lower character or underscore"]
      rest <- many $ do
        subsequent <- headParser
        guard $ isAlphaNum subsequent
        return subsequent
      let identifier = initial : rest
      guardNotReserved identifier
      return identifier
    rIdentifierParser = do
      initial <- headParser
      unless (isAlpha initial || initial == '_')
        $ failure ["expected a lower character or underscore"]
      rest <- many $ do
        subsequent <- headParser
        guard $ isAlphaNum subsequent
        return subsequent
      let identifier = initial : rest
      guardNotReserved identifier
      return identifier
    patternParser variableParser headVariableParser =
      VariablePattern
        <$> variableParser
        <|> ConstructorPattern
        <$> typeIdentifierParser
        <*> pure []
        <|> bracketed (headPatternParser variableParser headVariableParser)
    headPatternParser variableParser headVariableParser =
      ConstructorPattern
        <$> typeIdentifierParser
        <*> many (patternParser variableParser headVariableParser)
        <|> VariablePattern
        <$> headVariableParser
        <|> bracketed (headPatternParser variableParser headVariableParser)
        <|> patternParser variableParser headVariableParser
    letParser = do
      _    <- accept "let"
      _    <- some spaceParser
      let_ <-
        (do
          let parameterParser = do
                parameter <- lIdentifierParser
                _ <- many $ spaceParser
                _ <- accept ":"
                _ <- many $ spaceParser
                typeReference <- typeReferenceParser
                return $ (parameter, typeReference)
          identifier <- lIdentifierParser
          patterns   <-
            many
            $  some spaceParser
            *> patternParser (bracketed parameterParser) parameterParser
          return $ \source ->
            Let (VariablePattern identifier) $ foldr Function source patterns
        )
        <|> Let
        <$> patternParser lIdentifierParser lIdentifierParser
      _      <- many spaceParser
      _      <- accept "="
      _      <- many spaceParser
      source <- expressionParser
      _      <- some spaceParser
      _      <- accept "in"
      _      <- some spaceParser
      let_ source <$> expressionParser
    ifParser = do
      _     <- accept "if"
      _     <- some spaceParser
      if_   <- headTermParser
      _     <- some spaceParser
      _     <- accept "then"
      _     <- some spaceParser
      then_ <- headTermParser
      _     <- some spaceParser
      _     <- accept "else"
      _     <- some spaceParser
      else_ <- headTermParser
      return $ If if_ then_ else_
    termParser =
      bracketed expressionParser
        <|> primaryParser
        <|> Identifier
        <$> rIdentifierParser
        <|> unOpParser
        <|> letParser
        <|> ifParser
    headTermParser =
      foldl Application <$> termParser <*> many (some spaceParser *> termParser)
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
      , (LAssoc, [(some spaceParser, Application)])
      ]
      headTermParser
  in
    runParserUntilEof $ many spaceParser *> expressionParser <* many spaceParser

optimizeExpression :: EAst Integer String -> EAst Integer String
optimizeExpression (BinOp Sum value (Primary 0)) = optimizeExpression value
optimizeExpression (BinOp Sum (Primary 0) value) = optimizeExpression value
optimizeExpression (BinOp Mul (Primary 0) _) = Primary 0
optimizeExpression (BinOp Mul _ (Primary 0)) = Primary 0
optimizeExpression (BinOp Mul value (Primary 1)) = optimizeExpression value
optimizeExpression (BinOp Mul (Primary 1) value) = optimizeExpression value
optimizeExpression (BinOp operator left right) =
  let optimizedLeft  = optimizeExpression left
      optimizedRight = optimizeExpression right
  in  case optimizedLeft of
        Primary leftPrimary | Primary rightPrimary <- optimizedRight ->
          Primary $ binOperatorConstructor operator leftPrimary rightPrimary
        _ -> BinOp operator optimizedLeft optimizedRight
optimizeExpression (UnOp operator value) =
  let optimizedValue = optimizeExpression value
  in  case optimizedValue of
        Primary primaryValue ->
          Primary $ unOperatorConstructor operator primaryValue
        _ -> UnOp operator optimizedValue
optimizeExpression value = value

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
        <*> expressionParser
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
    runParserUntilEof $ many spaceParser *> expressionParser <* many spaceParser
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
          Function pattern expression ->
            printf "%s ->\n%s" (show pattern) (show' (ident n) expression)
          Application function argument -> printf "@\n%s\n%s"
                                                  (show' (ident n) function)
                                                  (show' (ident n) argument)
          Let pattern source target -> printf "let %s in\n%s\n=\n%s"
                                              (show pattern)
                                              (show' (ident n) source)
                                              (show' (ident n) target)
          If if_ then_ else_ -> printf "if\n%s\nthen\n%s\nelse\n%s"
                                       (show' (ident n) if_)
                                       (show' (ident n) then_)
                                       (show' (ident n) else_)
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
