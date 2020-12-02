module Main where

import System.IO
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let nums = map read numStrings :: [Integer]
      numStrings = words contents
  print $ product $ findPair nums
  hClose handle

findPair :: [Integer] -> [Integer]
findPair [] = []
findPair (a:as) = if length (filterHelp a as) == 1 then a : filterHelp a as else findPair as
  where filterHelp a = filter (\x -> x + a == 2020)
