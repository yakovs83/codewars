module CountChange where
import           Control.Monad
import           Data.List
import Data.Function

--first attempt, counts with repetitions
countChange::Int->[Int]->Int
countChange _ [] = 0
countChange 0 _ = 0
countChange a cs = let f c | a - c < 0 = 0
                           | a - c == 0 = 1
                           | a - c > 0 = countChange (a-c) cs
                   in sum $ map f cs


--works but runs for too long
countChange2::Int->[Int]->Int
countChange2 a cs = let wc = waysToCount a cs
                    in case wc of
                        [[]] -> 0
                        otherwise -> length . group . sort . map sort $ wc

tmp a cs = let wc = waysToCount a cs
               in nub . map sort $ wc

waysToCount::Int->[Int]->[[Int]]
waysToCount _ [] = [[]]
waysToCount 0 _  = [[]]
waysToCount a cs = do
    x <- map ((-) a) cs
    guard (not $ x < 0)
    let wtc = waysToCount x cs
    t <- nub . map sort $ wtc
    return $ (a-x):t

--more optimized version
countChange3 a cs = let r = map (\x->zipWith (*) [0..quot a x] $ repeat x) cs
                        comb [] = [[]]
                        comb r = [h:t| h<-head r, t<-comb $ tail r, h + sum t <= a]
                    in length $ filter (==a) $ map sum $ comb r
                    
main = do
    --print $ waysToCount 10 [3,2,4,1] 
    --print $ countChange2 10 [3,2,4,1]
    --print $ tmp 10 [3,2,4,1]
    print $ countChange3 300 [500,5,50,100,20,200,10]
    --print $ countChange3 10 [1,2,3] 