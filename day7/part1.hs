import qualified Data.IntSet as I
import System.IO
import Data.List (elemIndex)

findStart :: [String] -> (Int, [String])
findStart grid =
    let sRowI = maybe 0 id $ elemIndex True $ map (elem 'S') grid
        sRow = grid !! sRowI
        sCol = maybe 0 id $ elemIndex 'S' sRow
    in (sCol, drop (sRowI + 1) grid)

step :: (I.IntSet, Int) -> String -> (I.IntSet, Int)
step (beam, count) row =
    let splitter = I.fromList [i | (i, c) <- zip [0..] row, c == '^'] 
        hit = I.intersection beam splitter
        continue = I.difference beam hit
        split = I.unions [I.fromList [c-1, c+1] | c <- I.toList hit]
        nextBeam = I.union continue split
    in (nextBeam, count + I.size hit)


solve :: String -> Int
solve input =
    let grid = lines input
        (sCol, rows) = findStart grid
        (_, total) = foldl step (I.singleton sCol, 0) rows
    in total

main :: IO()
main = do
    file <- openFile "../inputs/day7.txt" ReadMode
    content <- hGetContents file
    print $ solve content
