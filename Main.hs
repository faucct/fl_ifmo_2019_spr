module Main where

import Data.List
import System.Environment
import Automaton
import Text.Printf

automatonInfo :: Automaton a b -> String
automatonInfo auto = 
  let [dfa, nfa, complete, minimal] = map (\f -> if f auto then "yes" else "no") [isDFA, isNFA, isComplete, isMinimal] in
  printf "Hurray! It's an automaton!\nDeterministic:    %s\nNondeterministic: %s\nComplete:         %s\nMinimal:          %s" dfa nfa complete minimal

main :: IO ()
main = do
  print $ parseAutomaton "<0,1><1><1><1><(1,0,1)>"
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseAutomaton input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either (printf "Not an automaton!\n%s" . intercalate "\n") automatonInfo a
        putStrLn ""
    ) 
    fileNames
