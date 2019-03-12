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
  let Right dfaAutomaton = parseAutomaton "<0,1><1><1><1><(1,0,1)>"
  print $ isDFA dfaAutomaton
  let Right epsilonAutomaton = parseAutomaton "<0,1><1><1><1><(1,\\epsilon,1)>"
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
