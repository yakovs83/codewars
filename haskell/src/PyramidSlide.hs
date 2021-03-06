module PyramidSlide where
import Data.List

longestSlideDown :: [[Int]] -> Int
longestSlideDown p = maximum $ foldl' (reducer) (head p) (tail p)

clean :: [Int] -> [Int] --remove lengths of duplicate paths, leaving only max length
clean r = case drop 3 r of
            [] -> r --we don't do anything for the list that is shorter than 3 elements
            _ -> head r : map (\(x,y) -> max x y) pr --otherwise we find pairwise max of the middle part of the list
            where pr = pairUp $ tail r

reducer :: [Int] -> [Int] -> [Int] --produce the row with the current length of slides from two rows
reducer r1 r2 = clean $ zipWith (+) (dupRow r1) (getMid . dupRow $ r2)

dupRow :: [Int] -> [Int] --duplicates list entries
dupRow r = concat . map (replicate 2) $ r

getMid :: [Int] -> [Int] --gets the middle part of the list, everything but first and last element
getMid r = init . tail $ r

pairUp :: [Int]->[(Int,Int)] --groups up elements pairwise into tuples
pairUp (h1:h2:t) = (h1,h2) : (pairUp t)
pairUp (h:[]) = [(h,0)]
pairUp [] = []

pt = [[3], [7, 4], [2, 4, 6], [8, 5, 9, 3]]
pt2 = [[75],
      [95, 64],
      [17, 47, 82],
      [18, 35, 87, 10],
      [20, 04, 82, 47, 65],
      [19, 01, 23, 75, 03, 34],
      [88, 02, 77, 73, 07, 63, 67],
      [99, 65, 04, 28, 06, 16, 70, 92],
      [41, 41, 26, 56, 83, 40, 80, 70, 33],
      [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
      [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
      [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
      [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
      [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
      [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

debugSlideDown :: [[Int]] -> [[Int]]
debugSlideDown p = let res pt = foldl' (reducer) (head pt) (tail pt)
                   in [res $ take n p | n <- [1..length p] ]

main = do
    print $ longestSlideDown pt
    mapM putStrLn . map show $ debugSlideDown pt
    print $ longestSlideDown pt2
    mapM putStrLn . map show $ debugSlideDown pt2
    