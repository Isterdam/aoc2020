module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split

main :: IO ()
main = do
  input <- readFile "input.txt"
  let nums = (map read . lines) input :: [Int]
  print $ findNotSum 25 nums

findNotSum :: Int -> [Int] -> Int
findNotSum i xs = if unique ([(x', y') | x' <- xs', y' <- xs', x' + y' == z]) then findNotSum (i + 1) xs else z
    where z = xs !! i
          xs' = take 25 . drop (i - 25) $ xs
          unique [] = False
          unique [(x, y)] = x /= y
          unique ((x, y):ps) = x /= y || unique ps