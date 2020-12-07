module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Map.Strict as Map 
import Data.Map (Map())
import Data.Maybe

-- possibly the ugliest and most inefficient solution possible. please don't look.
-- I should've used a graph for this :,)

type Bag = (String, [(Int, String)])

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle 
  let num = sum $ map (`traverse'` bagsMap) bags
      bagsMap = Map.fromList bags :: Map String [(Int, String)]
      bags = parseBags $ (map (splitWhen (=="contain") . words) . lines) contents
  print (num - 1) -- minus one because of "shiny gold" bag
  hClose handle

parseBags :: [[[String]]] -> [Bag]
parseBags [] = []
parseBags (b:bs)
  | "no" `elem` head contains = (bag, []) : parseBags bs
  | otherwise = (bag, parseContains contains) : parseBags bs
    where 
      bag = (head . head $ b) ++ " " ++ (head b !! 1)
      contains = splitWhen (`elem` ["bag,", "bags,"]) $ last b

parseContains :: [[String]] -> [(Int, String)] 
parseContains [b] = [(read $ head b :: Int, (b !! 1) ++ " " ++ (b !! 2))]
parseContains (b:bs) = (read $ head b :: Int, (b !! 1) ++ " " ++ (b !! 2)) : parseContains bs

traverse' :: Bag -> Map String [(Int, String)] -> Int
traverse' (s, []) _ = 0
traverse' (s, bs') bs
  | s == "shiny gold" = 1
  | 1 `elem` map (\b' -> traverse' (findBag bs b') bs) bs' = 1 -- check if "shiny gold" is contained in some of the sub-bags
  | otherwise = 0

findBag :: Map String [(Int, String)] -> (Int, String) -> Bag 
findBag bs (_, s) = (s, fromJust $ Map.lookup s bs)