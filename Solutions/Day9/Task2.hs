module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  input <- readFile "input.txt"
  let nums' = fromJust . head . filter isJust $ map (findContiguous 0 target) sets
      sets = contiguousSets nums
      target = findNotSum 25 nums
      nums = (map read . lines) input :: [Int]
  print $ minimum nums' + maximum nums'

findContiguous :: Int -> Int -> [Int] -> Maybe [Int]
findContiguous i target xs
    | sum xs' > target = Nothing
    | sum xs' < target = findContiguous (i + 1) target xs
    | otherwise = Just xs'
        where xs' = take i xs

contiguousSets :: [Int] -> [[Int]]
contiguousSets [] = []
contiguousSets xs'@(_:xs) = xs' : contiguousSets xs

findNotSum :: Int -> [Int] -> Int
findNotSum i xs = if unique ([(x', y') | x' <- xs', y' <- xs', x' + y' == z]) then findNotSum (i + 1) xs else z
    where z = xs !! i
          xs' = take 25 . drop (i - 25) $ xs
          unique [] = False
          unique [(x, y)] = x /= y
          unique ((x, y):ps) = x /= y || unique ps