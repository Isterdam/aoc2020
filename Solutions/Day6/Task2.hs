module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let total = sum $ map length commonGroups
      commonGroups = map (foldr1 intersect) groups
      groups = splitWhen (=="") $ lines contents
  print total
  hClose handle