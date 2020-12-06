module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  print $ sum . map ((length . nub) . concat) . splitWhen (=="") . lines $ contents
  hClose handle
