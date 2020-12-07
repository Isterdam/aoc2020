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
  let num = traverse' (findBag bagsMap "shiny gold") bagsMap
      bagsMap = Map.fromList bags :: Map String [(Int, String)]
      bags = parseBags $ (map (splitWhen (=="contain") . words) . lines) contents
  print num
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
traverse' (s, ss) bs = sum $ map (\(n',s') -> n' + n' * traverse' (findBag bs s') bs) ss 

findBag :: Map String [(Int, String)] -> String -> Bag 
findBag bs s = (s, fromJust $ Map.lookup s bs)