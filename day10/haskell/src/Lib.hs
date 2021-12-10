module Lib
    ( someFunc
    ) where

import Debug.Trace
import System.Environment
import Data.List

data Score = Completion Int
           | Error Int
           | Invalid
  deriving (Show)


isOpen :: Char -> Bool
isOpen c = case c of
  '(' -> True
  '[' -> True
  '{' -> True
  '<' -> True
  _ -> False

toClose :: Char -> Char
toClose '(' = ')'
toClose '[' = ']'
toClose '{' = '}'
toClose '<' = '>'
toClose x = x

errValue :: Char -> Int
errValue c = case c of
  ')' -> 3
  ']' -> 57
  '}' -> 1197
  '>' -> 25137
  _ -> 0

completionValue :: Char -> Int
completionValue c = case c of
  '(' -> 1
  '[' -> 2
  '{' -> 3
  '<' -> 4
  _ -> 0

completionScore :: String -> Int -> Int
completionScore (s:stack) acc =
  completionScore stack next
  where next = (5 * acc) + (completionValue s)
completionScore _ acc = acc

score :: String -> String -> Score
score stack (s:ss)
  | isOpen s =
      (score (s:stack) ss)
score (st:stack) (s:ss)
  | s == (toClose st) =
      (score stack ss)
score (st:stack) (s:ss) =
  Error (errValue s)
score stack [] =
  Completion (completionScore stack 0)
score _ _ = trace ("empty") Invalid

midpoint :: (Ord a) => [a] -> a
midpoint as = as !! mid
  where mid = len `div` 2
        len = length as

part1 :: String -> IO ()
part1 fname = do d <- readFile fname
                 print
                   $ sum
                   $ map (\s -> (case s of Error x -> x
                                           _ -> 0))
                   $ map (\l -> score "" l)
                   $ (lines d)

part2 :: String -> IO ()
part2 fname = do d <- readFile fname
                 print
                   $ midpoint
                   $ sort
                   $ filter (\s -> s /= 0)
                   $ map (\s -> (case s of Completion x -> x
                                           _ -> 0))
                   $ map (\l -> score "" l)
                   $ (lines d)



someFunc :: IO ()
someFunc = do
  part1 "../example.txt"
  part1 "../input.txt"
  part2 "../example.txt"
  part2 "../input.txt"
