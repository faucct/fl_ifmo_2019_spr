module Tokenizer where

import           Combinators
import           Control.Applicative
import           Data.Char
import           Data.Functor
import           Prelude                 hiding ( fail )

data Token = Ident String
           | KeyWord String
           | Number Int  -- Change Number type if you work with something other than Int
           deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize input = case runParser tokensParser input of
    Just (""  , tokens) -> tokens
    Just (rest, _     ) -> error $ "Unrecognized input: " ++ rest
    Nothing             -> error $ "Unrecognized input: " ++ input

spacingParser :: Parser String [Char]
spacingParser = some $ char ' '

tokenParser :: Parser String Token
tokenParser =
    fmap KeyWord parseKeyWord
        <|> fmap Ident  parseIdent
        <|> fmap Number parseNumber
tokensParser :: Parser String [Token]
tokensParser =
    (spacingParser <|> pure [])
        *> (   pure (:)
           <*> tokenParser
           <*> many (spacingParser *> tokenParser)
           <|> pure []
           )
        <* (spacingParser <|> pure [])

parseIdent :: Parser String String
parseIdent =
    success (:)
        <*> foldr ((<|>) . token) fail (['A' .. 'Z'] ++ ['a' .. 'z'])
        <*> many
                (foldr
                    ((<|>) . token)
                    fail
                    (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_'])
                )

parseKeyWord :: Parser String String
parseKeyWord = keywords
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
    ] <* not' (foldr
        ((<|>) . token)
        fail
        (['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '9'] ++ ['_'])
    )

parseNumber :: Parser String Int
parseNumber =
    (char '-' $> negate <|> (char '+' <|> pure '+') $> id)
        <*> (   pure (foldl (\rest digit -> rest * 10 + digitToInt digit) 0)
            <*> (   pure (:)
                <*> foldr ((<|>) . char) fail ['1' .. '9']
                <*> many
                        (  (char '_' <|> pure '_')
                        *> foldr ((<|>) . char) fail ['0' .. '9']
                        )
                )
            <|> pure digitToInt
            <*> foldr ((<|>) . char) fail ['0' .. '9']
            )
