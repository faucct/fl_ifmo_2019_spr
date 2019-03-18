module Main where

import           Data.Either
import           Data.Function
import           Data.List
import           System.Environment
import           Automaton
import           Combinators
import           Text.Printf
import qualified Data.Set                      as Set

automatonInfo :: Automaton String String -> String
automatonInfo auto =
  let [dfa, nfa, complete, minimal] = map
        (\f -> if f auto then "yes" else "no")
        [isDFA, isNFA, isComplete, isMinimal]
  in
    printf
      "Hurray! It's an automaton!\nDeterministic:    %s\nNondeterministic: %s\nComplete:         %s\nMinimal:          %s"
      dfa
      nfa
      complete
      minimal

main :: IO ()
main = do
  print
    $   (Automaton
          { sigma     = Set.fromList ["a", "b"]
          , states    = Set.fromList
                          [ Set.fromList []
                          , Set.fromList ["1"]
                          , Set.fromList ["1", "2"]
                          , Set.fromList ["2"]
                          ]
          , initState = Set.fromList ["1"]
          , termState = Set.fromList [Set.fromList ["2"]]
          , delta = [ ((Set.fromList [], "a")        , Set.fromList [])
                    , ((Set.fromList [], "b")        , Set.fromList [])
                    , ((Set.fromList ["1"], "a")     , Set.fromList ["1", "2"])
                    , ((Set.fromList ["1"], "b")     , Set.fromList ["1"])
                    , ((Set.fromList ["1", "2"], "a"), Set.fromList ["1", "2"])
                    , ((Set.fromList ["1", "2"], "b"), Set.fromList ["1", "2"])
                    , ((Set.fromList ["2"], "b")     , Set.fromList ["1", "2"])
                    ]
          } ==
        )
    .   determinized
    <$> parseAutomaton
          "<a, b><1, 2><1><2><(1, a, 1), (1, a, 2), (1, b, 1), (2, b, 1), (2, b, 2)>"

  print
    $   (Automaton { sigma     = Set.fromList []
                   , states    = Set.fromList ["a", "b"]
                   , initState = "a"
                   , termState = Set.fromList ["a"]
                   , delta     = []
                   } ==
        )
    .   closed
    <$> parseAutomaton "<><a, b><a><a><(a, \\epsilon, b)>"

  print
    $   (Automaton
          { sigma     = Set.fromList ["0", "1"]
          , states    = Set.fromList ["\\devil", "a"]
          , initState = "a"
          , termState = Set.fromList ["a"]
          , delta     = [ (("\\devil", "0"), "\\devil")
                        , (("\\devil", "1"), "\\devil")
                        , (("a", "0")      , "a")
                        , (("a", "1")      , "\\devil")
                        ]
          } ==
        )
    .   completed
    <$> parseAutomaton "<0, 1><a><a><a><(a, 0, a)>"

  print
    $   (Automaton
          { sigma     = Set.fromList ["0", "1"]
          , states    = Set.fromList ["A", "C", "D", "E", "F"]
          , initState = "A"
          , termState = Set.fromList ["F"]
          , delta     = [ (("A", "0"), "C")
                        , (("A", "1"), "A")
                        , (("C", "0"), "D")
                        , (("C", "1"), "D")
                        , (("D", "0"), "E")
                        , (("D", "1"), "F")
                        , (("E", "0"), "F")
                        , (("E", "1"), "F")
                        , (("F", "0"), "F")
                        , (("F", "1"), "F")
                        ]
          } ==
        )
    .   minimalized
    <$> parseAutomaton
          "<0, 1><A, B, C, D, E, F, G><A><G, F><(A, 0, C), (A, 1, B), (B, 0, C), (B, 1, A), (C, 0, D), (C, 1, D), (D, 0, E), (D, 1, F), (E, 0, F), (E, 1, G), (G, 0, G), (G, 1, F), (F, 0, F), (F, 1, F)>"

  let
    Right notDFAAutomaton =
      parseAutomaton
        "<a, b><1, 2, 3><1><1,3><(1, a, 1), (2, b, 1), (1, b, 2), (2, b, 2), (2, a, 3), (3, a, 2), (3, b, 3)>"
  print $ not $ isDFA notDFAAutomaton
  let Right dfaAutomaton = parseAutomaton "<0,1><1><1><1><(1,0,1)>"
  print $ isDFA dfaAutomaton
  let Right epsilonAutomaton = parseAutomaton "<0,1><1><1><1><(1,\\epsilon,1)>"
  print $ isNFA epsilonAutomaton
  print $ not $ isComplete epsilonAutomaton
  let Right completeAutomaton =
        parseAutomaton "<0,1><1><1><1><(1,0,1),(1,1,1)>"
  print $ isComplete completeAutomaton
  print
    $ lefts
    $ [ runParser
            (accept "foo\nw" *> Parser
              (\((lineAndNumber, _) : _) -> Left $ [show lineAndNumber])
            )
          $ withLineAndColumn "foo\nwtf"
      ]
  fileNames <- getArgs
  let checkComplete auto = isComplete $ completed $ determinized $ closed auto
  let checkMin = isMinimal . minimalized . determinized . closed
  print
    $   checkMin
    <$> parseAutomaton
          "<a, b, c, d><0, 1a, 2b, 2d, 3c, 4aa, 4bb, 5b0, 5d, 4db, 4ca, 5b4, 5b1><0><3c, 4aa, 4bb, 5b0, 5d, 4db, 4ca, 5b1>< (0, a, 1a),  (0, b, 2b),  (0, b, 1a),  (0, b,3c),  (0, c, 3c),  (0, d, 2d),  (1a, a, 4aa),  (2b, a, 5b0),  (2b, b, 4bb),  (2d, a, 5d),  (2d, b, 4db),  (3c, a, 4ca),  (5d, b, 5d),  (5b4, b, 5b0),  (5b0, b, 5b1)>"

  mapM_
    (\fileName -> do
      input <- readFile fileName
      let a = parseAutomaton input
      putStrLn $ printf "Parsing %s\n" fileName
      putStrLn $ either (printf "Not an automaton!\n%s" . intercalate "\n")
                        automatonInfo
                        a
      putStrLn ""
    )
    fileNames
