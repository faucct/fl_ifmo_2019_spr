module Main where

import           Combinators
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Map                       ( Map )
import qualified Data.Map                      as Map
import           System.Environment
import           Expression
import           Text.Printf
import qualified Data.Set                      as Set

main :: IO ()
main = do
  print $ parseExpression "((((((((125 > 5)))))))) || ((5^3)) < 6"
  print
    $   (BinOp Minus (BinOp Minus (Primary 1) (Primary 2)) (Primary 3) ==)
    <$> parseExpression "1-2-3"
  print $ parseExpression "1 && 1 || 2"
  print $ executeExpression "1&&2" <*> pure Map.empty
  print $ executeExpression "(1+2)*(3+4)\n" <*> pure Map.empty
  print $ parseExpression " 1 abc"
  print $ Right (UnOp Neg (Identifier "foo")) == parseExpression "-foo"
  print
    $   (-1 ==)
    <$> (executeExpression "-foo" <*> pure (Map.singleton "foo" 1))
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
  print
    $   (1 ==)
    <$> (executeExpression "  1 ^ (2 + 3) == 1 && 4 == 4  " <*> pure Map.empty)
  print $ (Primary 0 ==) . optimizeExpression <$> parseExpression "0 * foo"
  print $ (Primary 2 ==) . optimizeExpression <$> parseExpression "1 * 2"
  fileNames <- getArgs
  mapM_
    (\fileName -> do
      input <- readFile fileName
      let a = parseExpression input
      let r = executeExpression input
      putStrLn $ printf "Parsing %s\n" fileName
      putStrLn $ either show show a
      putStrLn $ either show (show . ($ Map.empty)) r
      putStrLn ""
    )
    fileNames
  print $ runParser typeSystemParser "data Empty"
  print $ runParser typeSystemParser "data Foo = Foo | Bar (((Foo))) Empty"
  print $ parseExpression "let function (Foo : Foo) = Bar Foo in function Foo"
  print
    $ parseExpression "let function (x : Foo) = Bar Foo in function Foo"
  print $ parseExpression "if 1 then 0 else 1"
  print $ do
    typeSystem <- runParserUntilEof
      typeSystemParser
      "data Empty; data Foo = Foo | Bar (((Foo))) Empty"
    parseExpression "let function (x : Foo) = Bar Foo in function Foo"
      >>= maybe (Left ["failed to infer"]) Right
      .   infer0 typeSystem
  print $ do
    typeSystem <- runParserUntilEof
      typeSystemParser
      "data Empty; data Foo = Foo | Bar (((Foo))) Empty"
    parseExpression "let function (Bar foo empty : Foo) = empty in function Foo"
      >>= maybe (Left ["failed to infer"]) Right
      .   infer0 typeSystem
  print
    $   parseExpression "if True then 0 else 1"
    >>= maybe (Left ["failed to infer"]) Right
    .   infer0 []
