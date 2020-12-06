module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let total = sum $ map length uniqueGroups
      uniqueGroups = map nub groups
      groups = (map concat . splitWhen (=="")) $ lines contents
  print total
  hClose handle
