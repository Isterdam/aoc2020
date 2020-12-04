module Main where

import System.IO
import Control.Monad
import Data.Char
import Data.List
import Data.List.Split

main :: IO ()
main = do
  handle <- openFile "input.txt" ReadMode
  contents <- hGetContents handle
  let validPassports = map validPassport passports
      passports = (map . concatMap) (endBy " ") splitLines
      splitLines = splitWhen (=="") $ lines contents
  print $ length [x | x <- validPassports, x]
  hClose handle

validPassport :: [String] -> Bool
validPassport xs = ((length xs == 8) || (length xs == 7 && not (cid xs))) && validFields xs
    where cid [] = False
          cid (x':x) = "cid" `isInfixOf` x' || cid x

eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

validFields :: [String] -> Bool
validFields [] = True
validFields (x:xs) = let x' = drop 4 x
                         digits = [c | c <- x', isDigit c]
                         n = read digits :: Int in
                     case take 3 x of "byr" -> n `elem` [1920..2002]
                                      "iyr" -> n `elem` [2010..2020]
                                      "eyr" -> n `elem` [2020..2030]
                                      "hgt" -> validHGT x' (read digits :: Int)
                                      "ecl" -> x' `elem` eyeColors
                                      "hcl" -> (head x' == '#') && all isHexDigit (tail x')
                                      "pid" -> (digits == x') && (length digits == 9)
                                      "cid" -> True
                                      _ -> False
                     && validFields xs

validHGT :: String -> Int -> Bool 
validHGT s hgt = let unit = [c | c <- s, isLetter c] in 
                 case unit of "cm" -> hgt `elem` [150..193]
                              "in" -> hgt `elem` [59..76]
                              _ -> False
