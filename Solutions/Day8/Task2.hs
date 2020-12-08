module Main where

import System.IO
import Control.Monad
import Data.List
import Data.List.Split
import Data.Maybe

type Instruction = (Int, String, Bool) -- bool denotes if instruction has been visited and int the accVal

main :: IO ()
main = do
  input <- readFile "input.txt"
  let acc = (sum . map fst') (fst program)
      program = run 0 (terminable 0 instructions)
      instructions = zipWith (\z (x,y) -> (z,x,y)) [0..] (zip lines' (replicate (length lines') False))
      lines' = lines input
  print acc

terminable :: Int -> [Instruction] -> [Instruction]
terminable index program = if isTerminable program' then program' else terminable (index + 1) program 
    where program' = take index program ++ [repl (program !! index)] ++ drop (index + 1) program
          isTerminable program = snd $ run 0 program
          repl (a, b, c) = case take 3 b of "jmp" -> (a, "nop" ++ drop 3 b, c)
                                            "nop" -> (a, "jmp" ++ drop 3 b, c)
                                            _ -> (a, b, c)

run :: Int -> [Instruction] -> ([Instruction], Bool) -- bool represents if program terminates
run index program 
  | index + 1 == length program = (visited program, True)
  | trd' (program !! index) = (visited program, False) -- upon visiting an already visited instruction, return visited instructions
  | otherwise = run nextIndex program'
    where nextIndex = index + fst result
          program' = take index program ++ [(snd result, snd' instruction, True)] ++ drop (index + 1) program -- visited
          result = read' instruction
          instruction = program !! index

-- reads an instruction and returns (#jumps, accVal)
read' :: Instruction -> (Int, Int) 
read' instruction = case take 3 (snd' instruction) of "jmp" -> (jumps, 0)
                                                      "acc" -> (1, accVal)
                                                      _ -> (1, 0)
                         where jumps = toInt (drop 4 (snd' instruction))
                               accVal = toInt (drop 4 (snd' instruction))
                               toInt ('+':ss') = read ss' :: Int 
                               toInt ('-':ss') = -(read ss' :: Int)

visited :: [Instruction] -> [Instruction]
visited [] = []
visited (i:is) = if trd' i then i : visited is else visited is

fst' (a, _, _) = a 
snd' (_, b, _) = b
trd' (_, _, c) = c