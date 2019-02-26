module Main where

import System.Environment
import Automaton

main :: IO ()
main = do
  [fileName] <- getArgs
  input <- readFile fileName
  let a = parseAutomaton' input
  putStrLn $ maybe "Not an automaton!" (const "Hurray! Correct automaton!") a
