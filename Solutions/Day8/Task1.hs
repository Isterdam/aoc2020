module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let visited = traverse' 0 g
      g = zipWith (\x y -> (x, fst y, snd y)) [0..(length instructions)] g' 
      g' = map (\i -> (i, False)) instructions
      instructions = convertToIndices . lines $ contents
  print $ sum $ acc visited (lines contents)
  hClose handle

traverse' :: Int -> [(Int, Int, Bool)] -> [(Int, Int, Bool)]
traverse' i ns
    | trd' (ns !! i) = takeAll ns 
    | not $ trd' (ns !! i) = traverse' (i + snd' (ns !! i)) flagged
        where flagged = take i ns ++ [(fst' (ns !! i), snd' (ns !! i), True)] ++ drop (i + 1) ns

convertToIndices :: [String] -> [Int]
convertToIndices [] = []
convertToIndices (s:ss) = case take 3 s of "jmp" -> toInt $ drop 4 s
                                           _ -> 1
                                        : convertToIndices ss

acc :: [(Int, Int, Bool)] -> [String] -> [Int] 
acc [] _ = [0]
acc (n:ns) ss = case take 3 (ss !! fst' n) of "acc" -> toInt $ drop 4 (ss !! fst' n)
                                              _ -> 0
                                           : acc ns ss

takeAll :: [(Int, Int, Bool)] -> [(Int, Int, Bool)]
takeAll [] = []
takeAll (x:xs) = if trd' x then x : takeAll xs else takeAll xs

toInt :: String -> Int
toInt ('+':ss') = read ss' :: Int 
toInt ('-':ss') = -(read ss' :: Int)

fst' (a, _, _) = a 
snd' (_, b, _) = b
trd' (_, _, c) = c