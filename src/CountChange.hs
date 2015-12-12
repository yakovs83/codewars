module CountChange where
import Control.Monad

countChange::Int->[Int]->Int
countChange _ [] = 0
countChange 0 _ = 0
countChange a cs = let f c | a - c < 0 = 0
                           | a - c == 0 = 1
                           | a - c > 0 = countChange (a-c) cs
                   in sum $ map f cs


countChange2::Int->[Int]->[[Int]]
countChange2 _ [] = [[]]
countChange2 a _ | a<0 = [[]]
countChange2 a cs = do
    x <- map ((-) a) cs
    t <- countChange2 x cs
    let res = a:x:t
    --guard (x < 0)
    --return x
    return res
    

main = do
    print $ countChange 4 [1,2]
    print $ countChange 10 [5,2,3]
    print $ countChange 11 [5,7]
    print $ countChange2 5 [1,2]