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
  fileNames <- getArgs
  mapM_
    (\fileName -> do
      input <- readFile fileName
      let a = parseExpression input
      putStrLn $ printf "Parsing %s\n" fileName
      putStrLn $ either id show a
      putStrLn ""
    )
    fileNames
