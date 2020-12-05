module Main where

import System.IO
import Control.Monad
import Data.Sort

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let cols = map (find [0..7]) . map (drop 7) $ lns
      rows = map (find [0..127]) . map (take 7) $ lns
      lns = lines contents
  print $ maximum $ zipWith (\row col -> (row*8)+col) rows cols
  hClose handle

find :: [Int] -> String -> Int 
find [r] _ = r
find rs (c:s)
    | c `elem` ['F', 'L'] = find (fst half) s
    | c `elem` ['B', 'R'] = find (snd half) s
        where half = splitAt ((length rs) `div` 2) rs
