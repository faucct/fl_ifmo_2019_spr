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
import           Data.Maybe
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

spaceParser
  :: (Alternative error, TokenContainer tokenContainer Char) => Parser [tokenContainer] (error a) Char
spaceParser = Parser $ \case
  symbol : rest | isSpace $ getToken symbol -> Right (rest, getToken symbol)
  _ -> Left empty

infixl 4 :@>
infixr 3 :->

newtype Fix f = Fix { unFix :: (f (Fix f))}

instance Eq (f (Fix f)) => Eq (Fix f) where
  (==) = (==) `on` unFix

instance Show (f (Fix f)) => Show (Fix f) where
  show (Fix f) = show f

data DataType identifier value = DataType identifier (Map identifier [TypeReference value]) deriving (Functor, Foldable, Traversable)

instance Eq identifier => Eq (DataType identifier value) where
  DataType dataType1 _ == DataType dataType2 _ = dataType1 == dataType2

instance Show identifier => Show (DataType identifier value) where
  show (DataType dataType _) = show dataType

intDataType = DataType "Int" Map.empty
boolDataType = DataType "Bool" $ Map.fromList [("True", []), ("False", [])]

data TypeReference value = TypeReference value | TypeReference value :@> TypeReference value | TypeReference value :-> TypeReference value deriving (Show, Eq, Functor, Foldable, Traversable)
intTypeReference = TypeReference $ Fix $ intDataType
boolTypeReference = TypeReference $ Fix $ boolDataType

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

dataDefinitionParser :: Parser String [String] (DataType String String)
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

type TypeSystem identifier value = [DataType identifier value]
typeSystemParser :: Parser String [String] (TypeSystem String String)
typeSystemParser =
  many spaceParser
    *> (   success []
       <*  eofParser
       <|> (:)
       <$> dataDefinitionParser
       <*  many spaceParser
       <*> manyUntil
             eofParser
             (  accept ";"
             *> many spaceParser
             *> dataDefinitionParser
             <* many spaceParser
             )
       )

data Pattern identifier pattern = VariablePattern pattern | ConstructorPattern identifier [Pattern identifier pattern] deriving (Eq, Show)

-- Simplest abstract syntax tree for expressions: only binops are allowed
data EAst a identifier = BinOp BinOperator (EAst a identifier) (EAst a identifier)
            | UnOp UnOperator (EAst a identifier)
            | Primary a
            | Identifier identifier
            | Function (Pattern identifier identifier) (TypeReference identifier) (EAst a identifier)
            | Application (EAst a identifier) (EAst a identifier)
            | Let (Pattern identifier identifier) (EAst a identifier) (EAst a identifier)
            | If (EAst a identifier) (EAst a identifier) (EAst a identifier)
        deriving (Eq)

bracketed
  :: (Applicative err, TokenContainer tokenContainer Char)
  => Parser [tokenContainer] (err String) a
  -> Parser [tokenContainer] (err String) a
bracketed parser = char '(' *> parser <* char ')'

spaced
  :: (Alternative err, TokenContainer tokenContainer Char)
  => Parser [tokenContainer] (err String) a
  -> Parser [tokenContainer] (err String) a
spaced parser = many spaceParser *> parser <* many spaceParser

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
    patternParser =
      VariablePattern
        <$> lIdentifierParser
        <|> ConstructorPattern
        <$> typeIdentifierParser
        <*> pure []
        <|> (bracketed $ spaced headPatternParser)
    headPatternParser =
      VariablePattern
        <$> lIdentifierParser
        <|> ConstructorPattern
        <$> typeIdentifierParser
        <*> many (some spaceParser *> patternParser)
        <|> (bracketed $ spaced headPatternParser)
    letParser = do
      _    <- accept "let"
      _    <- some spaceParser
      let_ <-
        (do
          let parameterParser = do
                parameter     <- headPatternParser
                _             <- many $ spaceParser
                _             <- accept ":"
                _             <- many $ spaceParser
                typeReference <- headTypeReferenceParser
                return $ (parameter, typeReference)
          identifier <- lIdentifierParser
          patterns   <-
            many
            $  some spaceParser
            *> (bracketed $ spaced parameterParser)
          return $ \source ->
            Let (VariablePattern identifier) $ foldr (uncurry Function) source patterns
        )
        <|> Let
        <$> patternParser
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

isArithmeticBinOperator :: BinOperator -> Bool
isArithmeticBinOperator Pow   = True
isArithmeticBinOperator Mul   = True
isArithmeticBinOperator Div   = True
isArithmeticBinOperator Sum   = True
isArithmeticBinOperator Minus = True
isArithmeticBinOperator _     = False

isEqualityBinOperator :: BinOperator -> Bool
isEqualityBinOperator Eq  = True
isEqualityBinOperator Neq = True
isEqualityBinOperator _   = False

isComparisonBinOperator :: BinOperator -> Bool
isComparisonBinOperator Le = True
isComparisonBinOperator Lt = True
isComparisonBinOperator Ge = True
isComparisonBinOperator Gt = True
isComparisonBinOperator _  = False

isLogicalBinOperator :: BinOperator -> Bool
isLogicalBinOperator Conj = True
isLogicalBinOperator Disj = True
isLogicalBinOperator _    = False

applyPattern
  :: (Ord identifier, Ord variable)
  => Pattern identifier variable
  -> TypeReference (Fix (DataType identifier))
  -> Maybe (Map variable (TypeReference (Fix (DataType identifier))))
applyPattern (VariablePattern variable) typeReference =
  Just $ Map.singleton variable typeReference
applyPattern (ConstructorPattern identifier patterns) (TypeReference (Fix (DataType _ constructors)))
  | Just parameters <- Map.lookup identifier constructors
  , length parameters == length patterns
  , Just envs <- sequenceA $ zipWith applyPattern patterns parameters
  = Just $ foldr Map.union Map.empty envs
applyPattern _ _ = Nothing

resolveTypeReference typeSystem = resolve where
  resolve (TypeReference identifier) = 
    TypeReference
      . Fix
      <$> (find (\(DataType dataType _) -> dataType == identifier) typeSystem)
  resolve (left :-> right) = (:->) <$> resolve left <*> resolve right
  resolve (left :@> right) = (:@>) <$> resolve left <*> resolve right

infer
  :: TypeSystem String (Fix (DataType String))
  -> (Map String (TypeReference (Fix (DataType String))))
  -> (EAst Integer String)
  -> Maybe (TypeReference (Fix (DataType String)))
infer typeSystem =
  let
    infer _ (Primary _) = Just $ intTypeReference
    infer context (BinOp op left right)
      | isArithmeticBinOperator op
      , Just True <- (intTypeReference ==) <$> infer context left
      , Just True <- (intTypeReference ==) <$> infer context right
      = Just intTypeReference
    infer context (BinOp op left right)
      | isEqualityBinOperator op
      , Just leftType <- infer context left
      , Just rightType <- infer context right
      , leftType == rightType
      = Just boolTypeReference
    infer context (BinOp op left right)
      | isComparisonBinOperator op
      , Just True <- (intTypeReference ==) <$> infer context left
      , Just True <- (intTypeReference ==) <$> infer context right
      = Just boolTypeReference
    infer context (BinOp op left right)
      | isLogicalBinOperator op
      , Just True <- (boolTypeReference ==) <$> infer context left
      , Just True <- (boolTypeReference ==) <$> infer context right
      = Just boolTypeReference
    infer context (If if_ then_ else_)
      | Just True <- (boolTypeReference ==) <$> infer context if_
      , Just thenType <- infer context then_
      , Just elseType <- infer context else_
      , thenType == elseType
      = Just thenType
    infer context (Identifier identifier)
      | Just type_ <- Map.lookup identifier context = Just $ type_
    infer context (Let pattern source target)
      | Just sourceType <- infer context source
      , Just env <- applyPattern pattern sourceType
      , Just targetType <- infer (Map.union env context) target
      = Just targetType
    infer context (Function pattern parameterType value)
      | Just resolvedParameterType <- resolveTypeReference typeSystem parameterType
      , Just env <- applyPattern pattern resolvedParameterType
      , Just valueType <- infer (Map.union env context) value
      = Just $ resolvedParameterType :-> valueType
    infer context (Application function argument)
      | Just (from :-> to) <- infer context function
      , Just argumentType <- infer context argument
      , argumentType == from
      = Just to
    infer _ _ = Nothing
  in
    infer

infer0
  :: [DataType [Char] String]
  -> EAst Integer String
  -> Maybe (TypeReference (Fix (DataType String)))
infer0 typeSystem expression = do
  let extendedTypeSystem = intDataType : boolDataType : typeSystem
      extractingIdentifiers (TypeReference identifier) = (identifier :)
      extractingIdentifiers (left :-> right) =
        ((.) `on` extractingIdentifiers) left right
      extractingIdentifiers (left :@> right) =
        ((.) `on` extractingIdentifiers) left right
      unresolvedIdentifiers =
        (foldr extractingIdentifiers [] $ foldr
            (\(DataType _ constructors) -> \prev -> foldr (++) prev constructors)
            []
            extendedTypeSystem
          )
          \\ map (\(DataType identifier _) -> identifier) typeSystem
  unless (null unresolvedIdentifiers) $ Nothing
  let resolve (TypeReference identifier) =
        TypeReference $ Fix $ (Map.!) indexedResolvedTypeSystem identifier
      resolve (left :-> right) = (resolve left :-> resolve right)
      resolve (left :@> right) = (resolve left :@> resolve right)
      indexedResolvedTypeSystem = Map.fromList $ map
        (\(DataType identifier constructors) ->
          (identifier, DataType identifier $ (map resolve) <$> constructors)
        )
        extendedTypeSystem
      resolvedTypeSystem = foldr (:) [] indexedResolvedTypeSystem

  infer
    resolvedTypeSystem
    (Map.unions $ map
      (\dataType@(DataType _ constructors) ->
        fmap (foldr (:->) (TypeReference $ Fix dataType)) constructors
      )
      resolvedTypeSystem
    )
    expression

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
          Function pattern parameterType expression ->
            printf "(%s : %s) ->\n%s" (show pattern) (show parameterType) (show' (ident n) expression)
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
