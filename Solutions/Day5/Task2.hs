module Main where

import System.IO
import Control.Monad
import Data.Sort

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let ids = sort $ zipWith (\row col -> (row*8)+col) rows cols
      cols = map (find [0..7] . drop 7) lns
      rows = map (find [0..127] . take 7) lns
      lns = lines contents
  print $ findGap ids (head ids - 1)
  hClose handle

find :: [Int] -> String -> Int 
find [r] _ = r
find rs (c:s)
    | c `elem` ['F', 'L'] = find (fst half) s
    | c `elem` ['B', 'R'] = find (snd half) s
        where half = splitAt (length rs `div` 2) rs

findGap :: [Int] -> Int -> Int
findGap (x:xs) prev = if x - 1 /= prev then x - 1 else findGap xs x
