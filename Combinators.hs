{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Combinators where

import           Control.Applicative
import           Data.Char
import           Data.Function                  ( on )
import           Data.List                      ( groupBy )

-- Parsing result is either an error message or some payload and a suffix of the input which is yet to be parsed    
newtype Parser str err ok = Parser { runParser :: str -> Either err (str, ok) }

data Assoc = LAssoc -- left associativity
           | RAssoc -- right associativity
           | NAssoc -- not associative

-- General parser combinator for expressions                      
-- Binary operators are listed in the order of precedence (from lower to higher)
-- Binary operators on the same level of precedence have the same associativity
-- Binary operator is specified with a parser for the operator itself and a semantic function to apply to the operands
expression
    :: Alternative err
    => [(Assoc, [(Parser String (err String) b, a -> a -> a)])]
    -> Parser String (err String) a
    -> Parser String (err String) a
expression associatedOperators primaryParser =
    let
        emptyStack = repeat Nothing
        withStack stack = do
            primary         <- primaryParser
            valueOrNewStack <- foldr
                (\((assoc, operators), prevOperator) higherPriorityParser -> do
                    valueOrNewStack <- higherPriorityParser
                    case valueOrNewStack of
                        Left higherOperator -> do
                            let leftPrimary = higherOperator primary
                            operatorConstructor <- foldr
                                (\(parser, constructor) ->
                                    ((  spaced parser
                                     *> success (Right constructor)
                                     ) <|>
                                    )
                                )
                                (success $ Left $ maybe higherOperator
                                                        (. higherOperator)
                                                        prevOperator
                                )
                                operators
                            traverse
                                (\operator ->
                                    ((: emptyStack) . Just)
                                        <$> maybe
                                                (return $ operator leftPrimary)
                                                (\prevOperator -> case assoc of
                                                    LAssoc ->
                                                        return
                                                            $ operator
                                                            $ prevOperator
                                                                  leftPrimary
                                                    NAssoc ->
                                                        failure
                                                            $ pure
                                                                  "non-associative operator"
                                                    RAssoc ->
                                                        return
                                                            $ prevOperator
                                                            . operator
                                                                  leftPrimary
                                                )
                                                prevOperator
                                )
                                operatorConstructor
                        Right newStack ->
                            return $ Right $ prevOperator : newStack
                )
                (pure $ Left id)
                (zip associatedOperators stack)
            case valueOrNewStack of
                Left  constructor -> return $ constructor primary
                Right newStack    -> withStack newStack
        space = Parser $ \case
            symbol : rest | isSpace symbol -> Right (rest, symbol)
            _                              -> Left empty
        spaced parser = many space *> parser <* many space
    in
        withStack emptyStack

eofParser :: Parser String [String] ()
eofParser = Parser $ \case
    ""   -> Right ("", ())
    rest -> Left ["Remaining input: " ++ rest]

runParserUntilEof :: Parser String [String] ok -> String -> Either [String] ok
runParserUntilEof parser input = snd <$> runParser (parser <* eofParser) input

withLineAndColumn :: [Char] -> [((Int, Int), Char)]
withLineAndColumn string = zip
    (scanl
        (\(line, column) symbol ->
            if symbol == '\n' then (line + 1, 0) else (line, column + 1)
        )
        (0, 0)
        string
    )
    string

class TokenContainer token t where
    getToken :: token -> t

instance TokenContainer Char Char where
    getToken = id

instance TokenContainer ((Int, Int), Char) Char where
    getToken = snd

-- Parser which always succeedes consuming no input
success :: ok -> Parser str err ok
success ok = Parser $ \s -> Right (s, ok)

-- Parser which fails no matter the input
failure :: err -> Parser str err ok
failure = Parser . const . Left

-- Biased choice: if the first parser succeedes, the second is never run
instance Alternative err => Alternative (Parser token (err e)) where
    empty = Parser $ const $ Left empty
    Parser u <|> Parser v = Parser f      where
        f xs = case u xs of
            Left errU -> case v xs of
                Left errV -> Left $ errU <|> errV
                z         -> z
            z -> z

-- Monadic sequence combinator
instance Monad (Parser token err) where
    return = success
    (Parser p) >>= q = Parser $ \str -> case p str of
        Right (rest, a) | Parser q <- q a -> q rest
        Left left                         -> Left left

-- Applicative sequence combinator
instance Applicative (Parser token err) where
    pure x = Parser $ \s -> Right (s, x)
-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
    Parser u <*> Parser v = Parser f      where
        f xs = case u xs of
            Right (xs', g) -> case v xs' of
                Right (xs'', x) -> Right (xs'', g x)
                Left  left      -> Left left
            Left left -> Left left

-- Applies a function to the parsing result, if parser succeedes
instance Functor (Parser token err) where
    fmap f (Parser parser) = Parser $ (fmap . fmap . fmap) f parser

-- Parses keywords 
keywords :: Alternative err => [String] -> Parser String (err String) String
keywords kws = (if any null kws then success "" else failure empty) <|> foldr
    ( (<|>)
    . (\keywordsGroup ->
          success (:) <*> token (head $ head keywordsGroup) <*> keywords
              (map tail keywordsGroup)
      )
    )
    (failure $ pure $ "expected a keyword: " ++ show kws)
    (groupBy ((==) `on` head) $ filter (not . null) kws)

-- Checks if the first element of the input is the given token
token
    :: ( Eq token
       , Show token
       , Applicative err
       , TokenContainer tokenContainer token
       )
    => token
    -> Parser [tokenContainer] (err String) token
token t = Parser $ \case
    (t' : s') | t == getToken t' -> Right (s', t)
    _                            -> Left $ pure $ "expected token: " ++ show t

not'
    :: Applicative err
    => Parser parsed (err String) ok
    -> Parser parsed (err String) ()
not' (Parser parser) = Parser
    (\parsed -> case parser parsed of
        Right _ -> Left $ pure $ "expected not to match"
        Left  _ -> Right (parsed, ())
    )

-- Checks if the first character of the string is the one given
char
    :: ( Eq token
       , Show token
       , Applicative err
       , TokenContainer tokenContainer token
       )
    => token
    -> Parser [tokenContainer] (err String) token
char = token

accept
    :: ( Alternative err
       , Eq token
       , Show token
       , TokenContainer tokenContainer token
       )
    => [token]
    -> Parser [tokenContainer] (err String) [token]
accept = foldr ((<*>) . (pure (:) <*>) . token) (success [])

headParser
    :: ( Eq token
       , Show token
       , Applicative err
       , TokenContainer tokenContainer token
       )
    => Parser [tokenContainer] (err String) token
headParser = Parser $ \case
    (token : rest) -> Right (rest, getToken token)
    _              -> Left $ pure "expected non-empty input"

manyUntil stopParser parser =
    (:) <$> parser <*> manyUntil stopParser parser <|> stopParser *> pure []
