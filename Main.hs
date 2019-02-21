module Main where

import Tokenizer

runTokenizer :: String -> IO ()
runTokenizer input = do
  putStrLn input
  print $ tokenize input
  putStrLn ""

main :: IO ()
main = do
  runTokenizer " 1 2 abc if "
  runTokenizer " "
  runTokenizer " f "
  runTokenizer "foo "
  runTokenizer "   "
  runTokenizer " -0 123 BEGIN foo"
  runTokenizer "defined"
  runTokenizer "12345678901234567890"
