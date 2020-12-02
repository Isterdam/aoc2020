-- finds the number of valid passwords according to a certain criterion
module Main where

import System.IO
import Control.Monad
import Data.List

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
checkPassword p = validPassword 0 (read (head p) :: Integer) (read (p !! 1) :: Integer) (head (p !! 2)) (last p)

-- checks if a passwords contains more than 'lo' of c', and less than 'high' of c'
validPassword :: Integer -> Integer -> Integer -> Char -> String -> Bool
validPassword i lo hi _ [] = (lo <= i) && (i <= hi)
validPassword i lo hi c' (c:s) = if c' == c then validPassword (i + 1) lo hi c' s else validPassword i lo hi c' s