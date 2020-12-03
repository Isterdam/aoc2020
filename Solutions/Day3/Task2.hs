module Main where

import System.IO
import Control.Monad

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let rows = tail $ lines contents -- skip first row
      cases = [(1,1), (3,1), (5,1), (7,1), (1,2)]
  print $ product $ map (squares rows (length $ head rows)) cases
  hClose handle

squares :: [String] -> Int -> (Int,Int) -> Int
squares rows rowLength (x,y)  = length [c | c <- sqs, c == '#'] 
    where sqs = zipWith (\s i -> s !! (i `mod` rowLength)) rows' [x,x*2..]
          rows' = every y rows

every :: Int -> [a] -> [a]
every n xs
    | length xs < n = []
    | otherwise = xs !! (n-1) : every n (drop n xs)