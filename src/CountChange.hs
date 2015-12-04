module CountChange where

countChange::Int->[Int]->Int
countChange _ [] = 0
countChange a cs = let f c | a - c < 0 = 0
                           | a - c == 0 = 1
                           | a - c > 0 = countChange (a-c) cs
                   in sum $ map f cs

main = do
    print $ countChange 4 [1,2]
    print $ countChange 10 [5,2,3]
    print $ countChange 11 [5,7]