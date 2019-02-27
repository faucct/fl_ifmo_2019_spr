{-# LANGUAGE LambdaCase #-}

module Automaton where

import           Combinators
import           Control.Applicative
import           Control.Monad
import           Data.Char
import qualified Data.Map.Lazy                 as Map
import qualified Data.Set                      as Set

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: Map (q, s) q
                               }

parseInteger :: Parser String Integer
parseInteger =
    char '0'
        *>  success 0
        <|> pure read
        <*> (   pure (:)
            <*> foldr ((<|>) . char) failure ['1' .. '9']
            <*> many
                    (  (char '_' <|> pure '_')
                    *> foldr ((<|>) . char) failure ['0' .. '9']
                    )
            )

parseSpace = Parser $ \string -> case string of
    (char : rest) | isSpace char -> Just (rest, char)
    _                            -> Nothing

parseList elem delim lbr rbr minimumNumberElems = do
    list <-
        lbr
        <*  many parseSpace
        *>  rbr
        *>  pure []
        <|> lbr
        <*  many parseSpace
        *>  pure (:)
        <*> elem
        <*  many parseSpace
        <*> many (delim *> many parseSpace *> elem <* many parseSpace)
        <*  rbr
    guard $ length list >= minimumNumberElems
    return list

-- Top level function: parses input string, checks that it is an automaton, and then returns it.
-- Should return Nothing, if there is a syntax error or the automaton is not a correct automaton.
-- This includes:
-- * The set of states is empty
-- * The init state is not a state
-- * Any of the terminal states is not a state
-- * Delta function is defined on not-a-state or not-a-symbol-from-sigma
-- Pick appropriate types for s and q
-- parseAutomaton :: String -> Maybe (Automaton ? ?)
parseAutomaton = fmap snd . runParser
    (   success Automaton
    <*> (   Set.fromList
        <$> parseList parseInteger (char ',') (char '<') (char '>') 0
        )
    <*> (   Set.fromList
        <$> parseList parseInteger (char ',') (char '<') (char '>') 1
        )
    <*> (char '<' *> parseInteger <* char '>')
    <*> (   Set.fromList
        <$> parseList parseInteger (char ',') (char '<') (char '>') 0
        )
    <*> (Map.fromList <$> parseList
            (   char '('
            <*  many parseSpace
            *>  pure (,)
            <*> (   pure (,)
                <*> parseInteger
                <*  many parseSpace
                <*  char ','
                <*  many parseSpace
                <*> parseInteger
                <*  many parseSpace
                )
            <*  char ','
            <*  many parseSpace
            <*> parseInteger
            <*  many parseSpace
            <*  char ')'
            )
            (char ',')
            (char '<')
            (char '>')
            0
        )
    )

parseAutomaton' =
    fmap snd
        . (runParser $ do
              sigma <- Set.fromList
                  <$> parseList parseInteger (char ',') (char '<') (char '>') 0
              states <- Set.fromList
                  <$> parseList parseInteger (char ',') (char '<') (char '>') 1
              initState <- char '<' *> parseInteger <* char '>'
              guard $ Set.member initState states
              termStates <- Set.fromList <$> parseList parseInteger
                                                       (char ',')
                                                       (char '<')
                                                       (char '>')
                                                       0
              guard $ all (`Set.member` states) termStates
              delta <- parseList
                  (   char '('
                  <*  many parseSpace
                  *>  pure (,)
                  <*> (   pure (,)
                      <*> parseInteger
                      <*  many parseSpace
                      <*  char ','
                      <*  many parseSpace
                      <*> parseInteger
                      <*  many parseSpace
                      )
                  <*  char ','
                  <*  many parseSpace
                  <*> parseInteger
                  <*  many parseSpace
                  <*  char ')'
                  )
                  (char ',')
                  (char '<')
                  (char '>')
                  0
              guard $ all
                  (\((from, symbol), to) ->
                      Set.member from states
                          && Set.member symbol sigma
                          && Set.member to states
                  )
                  delta
              return $ Automaton sigma
                                 states
                                 initState
                                 termStates
                                 (Map.fromList delta)
          )
