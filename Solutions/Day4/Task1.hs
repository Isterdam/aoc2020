module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split

data Passport = BYR Int | IYR Int | EYR Int | HGT String 
    | HCL String | ECL String | PID Int | CID Int

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
validPassport xs = (length xs == 8) || (length xs == 7 && not (cid xs))
    where cid [] = False
          cid (x':x) = isInfixOf "cid" x' || cid x