module Main where

import Data.List

main :: IO ()
main = do
  input <- readFile "input.txt"
  let threes = length $ filter (==3) jmps
      ones = length $ filter (==1) jmps
      jmps = jumps $ [0] ++ nums ++ [last nums + 3]
      nums = (sort . map read . lines) input :: [Int]
  print $ ones * threes

jumps :: [Int] -> [Int]
jumps [x, y] = [x, y - x]
jumps xs = jumps (init xs) ++ [last xs - last (init xs)]