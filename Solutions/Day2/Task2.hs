-- finds the number of valid passwords according to a certain criterion
module Main where

import System.IO
import Control.Monad

main :: IO ()
main = do
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle

    let validPasswords = length $ filter checkPassword passwords
        passwords = map words $ lines noHyphens :: [[String]]
        noHyphens = [if x == '-' then ' ' else x | x <- noColons]
        noColons = [x | x <- contents, x /= ':']

    print validPasswords
    hClose handle

checkPassword :: [String] -> Bool
checkPassword p = validPassword (read (head p) - 1 :: Int) (read (p !! 1) - 1 :: Int) (head (p !! 2)) (last p)

validPassword :: Int -> Int -> Char -> String -> Bool
validPassword i i' c' s
    | (s !! i) == c' && (s !! i') /= c' = True
    | (s !! i) /= c' && (s !! i') == c' = True
    | otherwise = False