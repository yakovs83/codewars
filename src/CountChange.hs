module CountChange where
import           Control.Monad
import           Data.List

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
    t <- waysToCount x cs
    return $ (a-x):t

countChange2::Int->[Int]->Int
countChange2 a cs = let wc = waysToCount a cs
                    in case wc of
                        [[]] -> 0
                        otherwise -> length . group . map sort $ wc

tmp a cs = let wc = waysToCount a cs
               in group . map sort $ wc

main = do
    print $ countChange 5 [1,2]
    print $ waysToCount 13 [5]
    print $ countChange2 13 [5]
    print $ tmp 13 [5]
