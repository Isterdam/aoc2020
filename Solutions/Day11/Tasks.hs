import Data.List.Split
import qualified Data.Map.Strict as Map 
import Data.Map (Map())
import Data.Maybe

type Chair = (Bool, [Int]) -- (Occupied, Adjacent Indices)

part1 :: IO ()
part1 = do 
    input <- readFile "input.txt"
    let chairs = Map.fromList $ findAdjacent (filter (not . snd) chairs') rowLen :: Map Int Chair
        chairs' = (concat . chunksOf rowLen) (zipWith (\x c -> if c == 'L' then (x, False) else (x, True)) [1..] lns)
        lns = concat (lines input)
        rowLen = (length . head . lines) input
    print $ occupiedSeats (untilStable chairs)

-- finds number of occupied seats
occupiedSeats :: Map Int Chair -> Int 
occupiedSeats m = length $ Map.filter (\c -> fst c) m

-- iterates until stable chair grid
untilStable :: Map Int Chair -> Map Int Chair
untilStable m = if m == m' then m' else untilStable m'
    where m' = occupyAll m

-- applies rules to all chairs
occupyAll :: Map Int Chair -> Map Int Chair
occupyAll m = Map.map (\c -> occupy c m) m

-- occupies and empties a chair according to rules
occupy :: Chair -> Map Int Chair -> Chair
occupy (b, is) m = if not b then -- not occupied
                       (if not (maximum bs) then True else False, is) -- rule 1
                   else
                       (if trus >= 4 then False else True, is) -- rule 2
    where trus = length $ filter (==True) bs
          bs = [fst (fromMaybe (False, []) (Map.lookup i m)) | i <- is]

-- finds adjacent chairs
findAdjacent :: [(Int, Bool)] -> Int -> [(Int, Chair)]
findAdjacent [] _ = []
findAdjacent ((x,b):cs) row = (x, (b, adjacent x row (row * row))) : findAdjacent cs row

-- finds all adjacent chairs for a certain chair
adjacent :: Int -> Int -> Int -> [Int]
adjacent x row len = 
    if x <= row || len - x > len - row then -- first or last row
        if x <= row then case x `mod` row of 0 -> [x-1, x+row, x+row-1]
                                             1 -> [x+1, x+row, x+row+1]
                                             _ -> [x-1, x+1, x+row, x+row-1, x+row+1] 
        else case x `mod` row of 0 -> [x-1, x-row, x-row-1]
                                 1 -> [x+1, x-row, x-row+1]
                                 _ -> [x-1, x+1, x-row, x-row-1, x-row+1]
    else -- any other row
        case x `mod` row of 0 -> [x-1, x-row, x-row-1, x+row, x+row-1]
                            1 -> [x+1, x-row, x-row+1, x+row, x+row+1]
                            _ -> [x-1, x+1, x-row, x-row-1, x-row+1, x+row, x+row-1, x+row+1]