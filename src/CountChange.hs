module CountChange where
import           Control.Monad
import           Data.List

--first attempt, counts with repetitions
countChange::Int->[Int]->Int
countChange _ [] = 0
countChange 0 _ = 0
countChange a cs = let f c | a - c < 0 = 0
                           | a - c == 0 = 1
                           | a - c > 0 = countChange (a-c) cs
                   in sum $ map f cs


waysToCount::Int->[Int]->[[Int]]
waysToCount _ [] = [[]]
waysToCount a _ | a==0 = [[]]
waysToCount a cs = do
    x <- map ((-) a) cs
    guard (not $ x < 0)
    let wtc = waysToCount x cs
    t <- nub . map sort $ wtc
    return $ (a-x):t

--works but runs for too long
countChange2::Int->[Int]->Int
countChange2 a cs = let wc = waysToCount a cs
                    in case wc of
                        [[]] -> 0
                        otherwise -> length . group . sort . map sort $ wc

tmp a cs = let wc = waysToCount a cs
               in nub . map sort $ wc

--more optimized version
countChange3::Int->[Int]->Int
countChange3 a cs = undefined 
                        


main = do
    print $ waysToCount 10 [3,2,4,1] 
    print $ countChange2 10 [3,2,4,1]
    print $ tmp 10 [3,2,4,1]
 