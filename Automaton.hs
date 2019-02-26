module Automaton where

import           Combinators
import           Control.Applicative
import           Control.Monad
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

parseList elem delim lbr rbr =
    lbr
        *>  rbr
        *>  pure []
        <|> lbr
        *>  pure (:)
        <*> elem
        <*> many (delim *> elem)
        <*  rbr

parseNotEmptyList elem delim lbr rbr =
    lbr *> pure (:) <*> elem <*> many (delim *> elem) <* rbr

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
        <$> parseList parseInteger (char ',') (char '<') (char '>')
        )
    <*> (Set.fromList <$> parseNotEmptyList parseInteger
                                            (char ',')
                                            (char '<')
                                            (char '>')
        )
    <*> (char '<' *> parseInteger <* char '>')
    <*> (   Set.fromList
        <$> parseList parseInteger (char ',') (char '<') (char '>')
        )
    <*> (Map.fromList <$> parseList
            (   char '('
            *>  pure (,)
            <*> (pure (,) <*> parseInteger <* char ',' <*> parseInteger)
            <*  char ','
            <*> parseInteger
            <*  char ')'
            )
            (char ',')
            (char '<')
            (char '>')
        )
    )

parseAutomaton' =
    fmap snd
        . (runParser $ do
              sigma <- Set.fromList
                  <$> parseList parseInteger (char ',') (char '<') (char '>')
              states <- Set.fromList <$> parseNotEmptyList parseInteger
                                                           (char ',')
                                                           (char '<')
                                                           (char '>')
              initState <- char '<' *> parseInteger <* char '>'
              guard $ Set.member initState states
              termStates <- Set.fromList <$> parseList parseInteger
                                                       (char ',')
                                                       (char '<')
                                                       (char '>')
              guard $ all (`Set.member` states) termStates
              delta <- parseList
                  (   char '('
                  *>  pure (,)
                  <*> (pure (,) <*> parseInteger <* char ',' <*> parseInteger)
                  <*  char ','
                  <*> parseInteger
                  <*  char ')'
                  )
                  (char ',')
                  (char '<')
                  (char '>')
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
