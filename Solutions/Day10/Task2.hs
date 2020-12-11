module Main where

import Data.List
import qualified Data.IntMap.Strict as IM
import Data.Maybe

main :: IO ()
main = do
  input <- readFile "input.txt"
  let m = formMap nums m'
      m' = IM.fromList [(0, 1)] :: IM.IntMap Int
      nums = [0] ++ nums' ++ [last nums' + 3]
      nums' = (sort . map read . lines) input :: [Int]
  print $ (fromJust . IM.lookup (last nums)) m 

reachable :: [Int] -> [Int]
reachable (x:xs) = takeWhile (<= x+3) xs

formMap :: [Int] -> IM.IntMap Int -> IM.IntMap Int 
formMap [] m = m
formMap (x:xs) m = formMap xs popMap
  where reaches = reachable (x:xs)
        paths = fromMaybe 0 (IM.lookup x m)
        -- #paths to x is sum of #paths to all y's that can reach x
        -- take keys from reaches, add #paths to value in map
        popMap = foldl (\m' k -> IM.insertWith (+) k paths m') m reaches


