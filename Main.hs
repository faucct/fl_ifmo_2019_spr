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
  print $ parseExpression "let function (Foo : Foo) = Bar Foo : Foo in function Foo"
  print
    $ parseExpression "let function (x : Foo) = Bar Foo : Empty -> Foo in function Foo"
  print $ parseExpression "if 1 then 0 else 1"
  print $ do
    typeSystem <- runParserUntilEof
      typeSystemParser
      "data Empty; data Foo = Foo | Bar (((Foo))) Empty"
    parseExpression "let function (x : Foo) = Bar Foo : Empty -> Foo in function Foo"
      >>= maybe (Left ["failed to infer"]) Right
      .   infer0 typeSystem
  print $ do
    typeSystem <- runParserUntilEof
      typeSystemParser
      "data Empty; data Foo = Foo | Bar (((Foo))) Empty"
    parseExpression "let function (Bar foo empty : Foo) = empty : Empty in function Foo"
      >>= maybe (Left ["failed to infer"]) Right
      .   infer0 typeSystem
  print
    $   parseExpression "if True then 0 else 1"
    >>= maybe (Left ["failed to infer"]) Right
    .   infer0 []
  print $ runParser typeSystemParser "data IntEndo = IntEndo (Int -> Int)"
  print $ do
    typeSystem <- runParserUntilEof
      typeSystemParser
      "data Nat = Z | S Nat; data List = Nil | Cons Nat List; data NatEndo = NatEndo (Nat -> Nat)"
    parseExpression "let id (z : Nat) = z : Nat in id Z"
      >>= maybe (Left ["failed to infer"]) Right
      .   infer0 typeSystem
  print $ do
    typeSystem <- runParserUntilEof
      typeSystemParser
      "data Nat = Z | S Nat; data List = Nil | Cons Nat List; data NatEndo = NatEndo (Nat -> Nat)"
    parseExpression "let id (z : Nat) = z : Nat in \
    \ let map (f : NatEndo) (l : List) = 13 : Int in \
    \ map (NatEndo id) (Nil)"
      >>= maybe (Left ["failed to infer"]) Right
      .   infer0 typeSystem
  print $ do
    typeSystem <- runParserUntilEof
      typeSystemParser
      "data Nat = Z | S Nat; data List = Nil | Cons Nat List; data NatEndo = NatEndo (Nat -> Nat)"
    parseExpression "let id (z : Nat) = z : Nat in \
      \ let map (NatEndo f : NatEndo) (Nil : List) = Nil : List in \
      \ let map (NatEndo f : NatEndo) (Cons hd tl : List) = Cons (f hd) (map (NatEndo f) tl) : List in \
      \ map (NatEndo id) (Cons Z (Cons (S Z) Nil))"
      >>= maybe (Left ["failed to infer"]) Right
      .   infer0 typeSystem
  print $ do
    typeSystem <- runParserUntilEof
      typeSystemParser
      "data Nat = Z | S Nat; data List = Nil | Cons Nat List; data NatEndo = NatEndo (Nat -> Nat)"
    parseExpression "let succ (x : Nat) = S x : Nat in \
      \ let lt (Z : Nat) (Z : Nat) = False : Bool in \
      \ let lt (Z : Nat) (S x : Nat) = True : Bool in \
      \ let lt (S x : Nat) (S y : Nat) = lt x y : Bool in \
      \ let makeList (x : Nat) (y : Nat) = makeList (succ x) y : List in \
      \ makeList Z Z"
      >>= maybe (Left ["failed to infer"]) Right
      .   infer0 typeSystem
  print $ parseExpression "1 -- comment\n + 2"
  if nestedCommentParsers
  then print $ parseExpression "1 {- {- comment\n\n\n -} -} + 2"
  else print $ parseExpression "1 {- {- comment\n\n\n -} + 2"
