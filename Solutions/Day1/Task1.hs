-- finds two integers in a given list that sum to 2020, and multiplies them
module Main where

import System.IO
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let nums = map read $ words contents :: [Int]
  print $ head [x*y | x <- nums, y <- nums, x+y==2020]
  hClose handle
