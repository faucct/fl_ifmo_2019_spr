module Main where

import           Data.Either
import           Data.List
import           System.Environment
import           Automaton
import           Combinators
import           Text.Printf

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
  print $ isLeft $ parseAutomaton "<a><b><b><b><(b, a, b)> , <a>\\n"
  let Right notDFAAutomaton = parseAutomaton "<a,b><0,1,2,3,4,5><0><1,3,4,5><(0, a, 1), (1, a, 2), (2, a, 3), (3, a, 4), (4, a, 5), (5, a, 0),  (0, a, 2), (1, a, 3), (2, a, 4), (3, a, 5), (4, a, 0), (5, a, 1),  (0, b, 5), (5, b, 4), (4, b, 3), (3, b, 2), (2, b, 1), (1, b, 0)>"
  print $ not $ isDFA notDFAAutomaton
  let Right dfaAutomaton = parseAutomaton "<a, b><1, 2, 3><1><1,3><(1, a, 1), (1, b, 2), (2, b, 2), (2, a, 3), (3, a, 2), (3, b, 3)>"
  print $ isDFA dfaAutomaton
  let Right notDFAAutomaton = parseAutomaton "<a, b><1, 2, 3><1><1,3><(1, a, 1), (2, b, 1), (1, b, 2), (2, b, 2), (2, a, 3), (3, a, 2), (3, b, 3)>"
  print $ not $ isDFA notDFAAutomaton
  let Right dfaAutomaton = parseAutomaton "<0,1><1><1><1><(1,0,1)>"
  print $ isDFA dfaAutomaton
  let Right epsilonAutomaton = parseAutomaton "<a><b><b><b><(b, a, b), (b, \\epsilon, b)>"
  print $ isNFA epsilonAutomaton
  print $ not $ isComplete epsilonAutomaton
  let Right completeAutomaton = parseAutomaton "<0,1><1><1><1><(1,0,1),(1,1,1)>"
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
