module Tokenizer where

import           Control.Applicative
import           Data.Char
import           Data.List

data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

newtype Parser a =
  Parser { runParser :: String ->  Maybe (String, a) }

instance Functor Parser where
  fmap f (Parser parser) = Parser $ (fmap . fmap . fmap) f parser

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)
  Parser u <*> Parser v = Parser f   where
    f xs = case u xs of
      Nothing       -> Nothing
      Just (xs', g) -> case v xs' of
        Nothing        -> Nothing
        Just (xs'', x) -> Just (xs'', g x)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  Parser u <|> Parser v = Parser f   where
    f xs = case u xs of
      Nothing -> v xs
      z       -> z

satisfy pr = Parser $ \string -> case string of
  (char : rest) | pr char -> Just (rest, char)
  _                       -> Nothing

repeated :: Parser a -> Parser [a]
repeated parser = pure (:) <*> parser <*> repeated parser <|> pure []

keywordParser = foldr (<|>) empty $ map
  (\keyword ->
    Parser $ fmap (\rest -> (rest, KeyWord keyword)) . stripPrefix keyword
  )
  [ "BEGIN"
  , "class"
  , "ensure"
  , "nil"
  , "self"
  , "when"
  , "END"
  , "def"
  , "false"
  , "not"
  , "super"
  , "while"
  , "alias"
  , "defined"
  , "for"
  , "or"
  , "then"
  , "yield"
  , "and"
  , "do"
  , "if"
  , "redo"
  , "true"
  , "begin"
  , "else"
  , "in"
  , "rescue"
  , "undef"
  , "break"
  , "elsif"
  , "module"
  , "retry"
  , "unless"
  , "case"
  , "end"
  , "next"
  , "return"
  , "until"
  ]

identParser :: Parser Token
identParser =
  pure Ident
    <*> (   pure (:)
        <*> satisfy
              (\char -> char `elem` ['A' .. 'Z'] || char `elem` ['a' .. 'z'])
        <*> repeated
              (satisfy $ \char ->
                char
                  `elem` ['A' .. 'Z']
                  ||     char
                  `elem` ['a' .. 'z']
                  ||     char
                  `elem` ['0' .. '9']
                  ||     char
                  ==     '_'
              )
        )

numberParser :: Parser Token
numberParser =
  pure Number
    <*> (   (   satisfy (== '-')
            *>  pure negate
            <|> (satisfy (== '+') <|> pure '+')
            *>  pure id
            )
        <*> (   pure (foldl ((\rest digit -> rest * 10 + digitToInt digit)) 0)
            <*> (   pure (:)
                <*> satisfy (`elem` ['1' .. '9'])
                <*> repeated
                      (  (satisfy (== '_') <|> pure '_')
                      *> satisfy (`elem` ['0' .. '9'])
                      )
                )
            <|> pure digitToInt
            <*> satisfy (`elem` ['0' .. '9'])
            )
        )

spacingParser = pure (:) <*> satisfy (== ' ') <*> repeated (satisfy (== ' '))

tokenParser = keywordParser <|> identParser <|> numberParser
tokensParser =
  (spacingParser <|> pure [])
    *> (   pure (:)
       <*> tokenParser
       <*> repeated (spacingParser *> tokenParser)
       <|> pure []
       )
    <* (spacingParser <|> pure [])

main = do
  putStrLn $ show $ tokenize "   "
  putStrLn $ show $ tokenize " f "
  putStrLn $ show $ tokenize "foo "
  putStrLn $ show $ tokenize " -0 123 BEGIN foo"

tokenize :: String -> [Token]
tokenize input = case runParser tokensParser input of
  Just (""  , tokens) -> tokens
  Just (rest, _     ) -> error $ "Unrecognized input: " ++ rest
  Nothing             -> error $ "Unrecognized input: " ++ input
