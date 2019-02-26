{-# LANGUAGE LambdaCase #-}

module Combinators where

import           Control.Applicative
import           Data.Function                  ( on )
import           Data.List                      ( groupBy )

-- Parsing result is some payload and a suffix of the input which is yet to be parsed
newtype Parser str ok = Parser { runParser :: str -> Maybe (str, ok) }

-- Parser which always succeedes consuming no input
success :: ok -> Parser str ok
success ok = Parser $ \s -> Just (s, ok)

-- Parser which fails no matter the input
failure :: Parser str ok
failure = Parser $ const Nothing

-- Biased choice: if the first parser succeedes, the second is never run
instance Alternative (Parser token) where
  empty = Parser $ const Nothing
  Parser u <|> Parser v = Parser f   where
    f xs = case u xs of
      Nothing -> v xs
      z       -> z

-- Default sequence combinator
-- If the first parser succeedes then the second parser is used
-- If the first does not succeed then the second one is never tried
-- The result is collected into a pair
seq :: Parser str a -> Parser str b -> Parser str (a, b)
(Parser p) `seq` (Parser q) = Parser $ \str -> case p str of
  Just (rest, a) | Just (rest, b) <- q rest -> Just (rest, (a, b))
  _ -> Nothing

-- Monadic sequence combinator
instance Monad (Parser token) where
  return = success
  (Parser p) >>= q = Parser $ \str -> case p str of
    Just (rest, a) | Parser q <- q a -> q rest
    _ -> Nothing

-- Applicative sequence combinator
instance Applicative (Parser token) where
  pure x = Parser $ \s -> Just (s, x)
  Parser u <*> Parser v = Parser f   where
    f xs = case u xs of
      Nothing       -> Nothing
      Just (xs', g) -> case v xs' of
        Nothing        -> Nothing
        Just (xs'', x) -> Just (xs'', g x)

-- Applies a function to the parsing result, if parser succeedes
instance Functor (Parser token) where
  fmap f (Parser parser) = Parser $ (fmap . fmap . fmap) f parser

-- Parses keywords 
keywords :: [String] -> Parser String String
keywords kws = (if any null kws then success "" else failure) <|> foldr
  ( (<|>)
  . (\keywordsGroup ->
      success (:) <*> token (head $ head keywordsGroup) <*> keywords
        (map tail keywordsGroup)
    )
  )
  failure
  (groupBy ((==) `on` head) $ filter (not . null) kws)

-- Checks if the first element of the input is the given token
token :: Eq token => token -> Parser [token] token
token t = Parser $ \case
  (t' : s') | t == t' -> Just (s', t)
  _                   -> Nothing

not' :: Parser parsed ok -> Parser parsed ()
not' (Parser parser) = Parser (\parsed -> case parser parsed of Just _ -> Nothing; Nothing -> Just (parsed, ()))

-- Checks if the first character of the string is the one given
char :: Char -> Parser String Char
char = token

accept :: Eq token => [token] -> Parser [token] [token]
accept = foldr ((<*>) . (pure (:) <*>) . token) (success [])
