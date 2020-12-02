-- finds two integers in a given list that sum to 2020, and multiplies them
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
findPair (a:as) = if length (filterHelp a as) == 2 then filterHelp a as else findPair as
  where filterHelp a as = a : filter (\x -> x + a == 2020) as -- finds any possible pair
