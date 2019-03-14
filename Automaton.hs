{-# LANGUAGE FlexibleContexts #-}

module Automaton where

import           Combinators
import           Control.Applicative
import           Control.Monad
import           Control.Monad.State
import           Data.Char
import           Data.Function
import           Data.List
import qualified Data.Map.Lazy                 as Map
import           Data.Maybe
import qualified Data.Set                      as Set
import           Data.Tuple

type Set = Set.Set
type Map = Map.Map

data Automaton s q = Automaton { sigma     :: Set s
                               , states    :: Set q
                               , initState :: q
                               , termState :: Set q
                               , delta     :: [((q, s), q)]
                               } deriving (Show, Eq)

parseElement :: (Alternative err) => Parser String (err String) String
parseElement = some
    (Parser $ \string -> case string of
        (symbol : rest) | not (isSpace symbol) && notElem symbol "(),<>" ->
            Right (rest, symbol)
        _ -> Left $ pure "expected element"
    )

parseSpace = Parser $ \string -> case string of
    (char : rest) | isSpace char -> Right (rest, char)
    _                            -> Left empty

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
parseAutomaton :: String -> Either [String] (Automaton String String)
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
                          && (symbol == "\\epsilon" || Set.member symbol sigma)
                          && Set.member to states
                  )
                  delta
              return $ Automaton sigma states initState termStates delta
          )

-- Checks if the automaton is deterministic (only one transition for each state and each input symbol)
isDFA :: Automaton String String -> Bool
isDFA (Automaton _ _ _ _ delta) =
    (all ((/= "\\epsilon") . snd . fst) delta)
        && (all ((== 1) . length) $ groupBy ((==) `on` fst) $ sortBy
               (compare `on` fst)
               delta
           )

-- Checks if the automaton is nondeterministic (eps-transition or multiple transitions for a state and a symbol)
isNFA :: Automaton String String -> Bool
isNFA = const True

-- Checks if the automaton is complete (there exists a transition for each state and each input symbol)
isComplete :: Automaton String String -> Bool
isComplete automaton@(Automaton sigma states _ _ delta) = all
    (\symbol -> all (\state -> any ((== (state, symbol)) . fst) delta) states)
    sigma

transitiveClosure :: Eq a => [(a, a)] -> [(a, a)]
transitiveClosure closure | closure == closureUntilNow = closure
                          | otherwise = transitiveClosure closureUntilNow
  where
    closureUntilNow =
        nub
            $  closure
            ++ [ (a, c) | (a, b) <- closure, (b', c) <- closure, b == b' ]

mergeStates state1 state2 automaton@(Automaton sigma states initialState terminalStates delta)
    = let replaceState state = if state == state2 then state1 else state
      in  Automaton
              sigma
              (Set.map replaceState states)
              (replaceState initialState)
              (Set.map replaceState terminalStates)
              (nub $ map
                  (\((from, symbol), to) ->
                      ((replaceState from, symbol), replaceState to)
                  )
                  delta
              )

completed automaton@(Automaton sigma states initialState terminalStates delta)
    = if isDFA automaton
        then if length delta == Set.size sigma * Set.size states
            then automaton
            else
                let newStates = Set.insert "\\devil" states
                in
                    Automaton
                        sigma
                        newStates
                        initialState
                        terminalStates
                        (   (\pair -> fromMaybe (pair, "\\devil")
                                $ find ((== pair) . fst) delta
                            )
                        <$> liftA2 (,)
                                   (Set.toList newStates)
                                   (Set.toList sigma)
                        )
        else error "is not DFA"

determinized automaton@(Automaton sigma states initialState terminalStates delta)
    = if any ((== "\\epsilon") . snd . fst) delta
        then error "epsilon transitions"
        else
            let
                initialTransitions = Map.fromListWith Set.union
                    $ map (\(pair, to) -> (pair, Set.singleton to)) delta
                newTransitions = execState
                    (do
                        let
                            addNewState newState = do
                                isNew <- gets $ not . Map.member newState
                                when isNew $ do
                                    modify $ Map.insert newState Map.empty
                                    mapM_
                                        (\symbol -> do
                                            let
                                                to = foldr
                                                    (\oldFrom to ->
                                                        maybe to (Set.union to)
                                                            $ Map.lookup
                                                                  ( oldFrom
                                                                  , symbol
                                                                  )
                                                                  initialTransitions
                                                    )
                                                    Set.empty
                                                    newState
                                            modify $ Map.update
                                                (Just . Map.insert symbol to)
                                                newState
                                            addNewState to
                                        )
                                        sigma
                        mapM_ addNewState $ Map.elems initialTransitions
                    )
                    (Map.fromListWith Set.union <$> Map.fromListWith
                        (++)
                        (map
                            (\((from, symbol), to) ->
                                ( Set.singleton from
                                , [(symbol, Set.singleton to)]
                                )
                            )
                            delta
                        )
                    )
            in
                Automaton
                    sigma
                    (Set.fromList $ Map.keys newTransitions)
                    (Set.singleton initialState)
                    (Set.map Set.singleton terminalStates)
                    ( concatMap
                            (\(from, transitions) ->
                                (\(symbol, to) -> ((from, symbol), to))
                                    <$> Map.toList transitions
                            )
                    $ Map.toList newTransitions
                    )

closed automaton@(Automaton sigma states initialState terminalStates delta) =
    let
        epsilonClosureDelta =
            transitiveClosure
                $ map (\((from, symbol), to) -> (from, to))
                $ filter (\((from, symbol), to) -> symbol == "\\epsilon") delta
        newTerminal = foldr
            (\(from, to) newTerminal -> if elem to terminalStates
                then Set.insert from newTerminal
                else newTerminal
            )
            terminalStates
            epsilonClosureDelta
        newDelta =
            filter (\((_, symbol), _) -> symbol /= "\\epsilon") $ nub $ foldr
                (\(u, v) newDelta ->
                    map (\((_, symbol), w) -> ((u, symbol), w))
                        $  filter
                               (\((from, symbol), _) ->
                                   from == v && symbol == "\\epsilon"
                               )
                               delta
                        ++ delta
                )
                delta
                epsilonClosureDelta
    in
        Automaton sigma states initialState newTerminal newDelta

minimalized automaton@(Automaton sigma states initialState terminalStates delta)
    = if isDFA automaton
        then
            let
                inverseDeltas = Map.fromListWith (++) <$> Map.fromListWith
                    (++)
                    (map (\((from, symbol), to) -> (to, [(symbol, [from])]))
                         delta
                    )
                initialQueue = uncurry (liftA2 (,))
                    $ partition (`elem` terminalStates) (Set.toList states)
                initialTable =
                    Set.fromList $ initialQueue ++ map swap initialQueue
                run
                    :: State
                           ([(String, String)], Set (String, String))
                           (Set (String, String))
                run = do
                    maybeList <- gets $ listToMaybe . fst
                    if isNothing maybeList
                        then gets snd
                        else do
                            (stateA, stateB) <- state
                                (\(pair : rest, table) -> (pair, (rest, table)))
                            mapM_
                                (\symbol -> do
                                    let
                                        pairs =
                                            (    liftA2 (,)
                                                `on` ( fromMaybe []
                                                     . Map.lookup symbol
                                                     . fromMaybe Map.empty
                                                     . (`Map.lookup` inverseDeltas
                                                       )
                                                     )
                                                )
                                                stateA
                                                stateB
                                    mapM_
                                        (\pair -> do
                                            (queue, table) <- get
                                            unless (Set.member pair table)
                                                $ put
                                                      ( pair : queue
                                                      , Set.insert pair
                                                          $ Set.insert
                                                                (swap pair)
                                                                table
                                                      )
                                        )
                                        pairs
                                )
                                (Set.toList sigma)
                            run
            in
                foldr (uncurry mergeStates) automaton
                . (liftA2 (,) (Set.toList states) (Set.toList states) \\)
                . Set.toList
                . snd
                $ execState run (initialQueue, initialTable)
        else error "is not DFA"

-- Checks if the automaton is minimal (only for DFAs: the number of states is minimal)
isMinimal :: Automaton String String -> Bool
isMinimal automaton = minimalized automaton == automaton
