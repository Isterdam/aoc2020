-- finds three integers in a given list that sum to 2020, and multiplies them
module Main where

import System.IO
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let nums = map read numStrings :: [Int]
      numStrings = words contents
  print $ head [x*y*z | x <- nums, y <- nums, z <- nums, x+y+z==2020]
  hClose handle
