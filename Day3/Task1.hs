module Main where

import System.IO
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let rows = tail $ lines contents -- skip first row
      rowLength = length $ head rows
      squares = zipWith (\s i -> s !! (i `mod` rowLength)) rows [3,6..]
  print $ length [x | x <- squares, x == '#']
  hClose handle