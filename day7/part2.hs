import qualified Data.IntSet as I
import qualified Data.IntMap as S
import System.IO
import Data.List (elemIndex)

findStart :: [String] -> (Int, [String])
findStart grid =
    let sRowI = maybe 0 id $ elemIndex True $ map (elem 'S') grid
        sRow = grid !! sRowI
        sCol = maybe 0 id $ elemIndex 'S' sRow
    in (sCol, drop (sRowI + 1) grid)

step :: S.IntMap Int -> String -> S.IntMap Int
step tms row =
    let splitter = I.fromList [i | (i, c) <- zip [0..] row, c == '^'] 
        updateNext tm col count
            | col `I.member` splitter = 
                let left = S.insertWith (+) (col - 1) count tm
                    right = S.insertWith (+) (col + 1) count left
                in right
            | otherwise = S.insertWith (+) col count tm
    in S.foldlWithKey updateNext S.empty tms


solve :: String -> Int
solve input =
    let grid = lines input
        (sCol, rows) = findStart grid
        iMap = S.singleton sCol 1
        fMap = foldl step iMap  rows
    in sum (S.elems fMap)

main :: IO()
main = do
    file <- openFile "../inputs/day7.txt" ReadMode
    content <- hGetContents file
    print $ solve content
