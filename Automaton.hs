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

parseElement = some
    (Parser $ \string -> case string of
        (symbol : rest) | not (isSpace symbol) && notElem symbol "(),<>" ->
            Just (rest, symbol)
        _ -> Nothing
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
parseAutomaton :: String -> Either String (Automaton s q)
parseAutomaton =
    fmap snd
    . (runParser $ do
          sigma <- Set.fromList
              <$> parseList parseElement (char ',') (char '<') (char '>') 0
          states <- Set.fromList
              <$> parseList parseElement (char ',') (char '<') (char '>') 1
          initState <- char '<' *> parseElement <* char '>'
          guard $ Set.member initState states
          termStates <- Set.fromList <$> parseList parseElement
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
                  <*> parseElement
                  <*  many parseSpace
                  <*  char ','
                  <*  many parseSpace
                  <*> parseElement
                  <*  many parseSpace
                  )
              <*  char ','
              <*  many parseSpace
              <*> parseElement
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



-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton a b -> Bool
isDFA = undefined

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton a b -> Bool
isNFA = undefined

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Automaton a b -> Bool 
isComplete = undefined

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton a b -> Bool
isMinimal = undefined
