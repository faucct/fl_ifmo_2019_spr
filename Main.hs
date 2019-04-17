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
    $   (BinOp
          Conj
          (BinOp
            Gt
            (BinOp
              Pow
              (Primary 10)
              (BinOp Pow
                     (BinOp Sum (Primary 1) (Primary 2))
                     (BinOp Sum (Primary 5) (Primary 5))
              )
            )
            (Primary 3)
          )
          (BinOp
            Le
            (BinOp
              Div
              (BinOp Div
                     (BinOp Div (BinOp Div (Primary 1) (Primary 2)) (Primary 3))
                     (Primary 4)
              )
              (Primary 5)
            )
            (Primary 0)
          ) ==
        )
    <$> parseExpression "(10)^(1+2)^(5+5) > 3 && 1/2/3/4/5 <= 0"
  print $ parseExpression "1&&0||1&&1"
  print $ parseExpression ""
  print $ Left ["remaining input: abc"] == parseExpression " 1 abc"
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
  fileNames <- getArgs
  mapM_
    (\fileName -> do
      input <- readFile fileName
      let a = parseExpression input
      putStrLn $ printf "Parsing %s\n" fileName
      putStrLn $ either show show a
      putStrLn ""
    )
    fileNames
