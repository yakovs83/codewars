module PyramidSlide where
import Data.List

longestSlideDown :: [[Int]] -> Int
longestSlideDown p = let reducer r1 r2 = zipWith (+) (dupRow r1) (getMid . dupRow $ r2)
            in maximum $ foldl' (reducer) (head p) (tail p)

dupRow :: [Int] -> [Int]
dupRow r = concat . map (replicate 2) $ r

getMid :: [Int] -> [Int]
getMid r = init . tail $ r

pt = [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]]

main = do
    print $ longestSlideDown pt
    