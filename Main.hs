module Main where

import           Data.Either
import           Data.Function
import           Data.List
import           System.Environment
import           Expression
import           Text.Printf
import qualified Data.Set                      as Set

main :: IO ()
main = do
  print
    $   (BinOp Minus (BinOp Minus (Primary 1) (Primary 2)) (Primary 3) ==)
    <$> parseExpression "1-2-3"
  print $ parseExpression "1 && 1 || 2"
  print $ Left ["Remaining input: abc"] == parseExpression " 1 abc"
  print
    $   (BinOp
          Conj
          (BinOp Eq
                 (BinOp Pow (Primary 1) (BinOp Sum (Primary 2) (Primary 3)))
                 (Primary 1)
          )
          (BinOp Eq (Primary 4) (Primary 4)) ==
        )
    <$> parseExpression "  1 ^ (2 + 3) == 1 && 4 == 4  "
  print $ (1 ==) <$> executeExpression "  1 ^ (2 + 3) == 1 && 4 == 4  "
  fileNames <- getArgs
  mapM_
    (\fileName -> do
        input <- readFile fileName
        let a = parseExpression input
        let r = executeExpression input
        putStrLn $ printf "Parsing %s\n" fileName
        putStrLn $ either show show a
        putStrLn $ either show show r
        putStrLn ""
    )
    fileNames
