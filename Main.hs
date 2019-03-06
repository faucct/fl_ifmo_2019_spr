module Main where

import System.Environment
import Automaton
import Text.Printf

automatonInfo :: Automaton a b -> String
automatonInfo auto = 
  let [dfa, nfa, complete, minimal] = map (\f -> if f auto then "yes" else "no") [isDFA, isNFA, isComplete, isMinimal] in
  printf "Hurray! It's an automaton!\nDeterministic:    %s\nNondeterministic: %s\nComplete:         %s\nMinimal:          %s" dfa nfa complete minimal

main :: IO ()
main = do
<<<<<<< HEAD
  [fileName] <- getArgs
  input <- readFile fileName
  let a = parseAutomaton' input
  putStrLn $ maybe "Not an automaton!" (const "Hurray! Correct automaton!") a
||||||| merged common ancestors
  [fileName] <- getArgs
  input <- readFile fileName
  let a = parseAutomaton input
  putStrLn $ maybe "Not an automaton!" (const "Hurray! Correct automaton!") a
=======
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseAutomaton input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either (printf "Not an automaton!\n%s") automatonInfo a
        putStrLn ""
    ) 
    fileNames
>>>>>>> upstream/HW04
