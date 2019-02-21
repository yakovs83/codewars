module InfiniteMatrix where
type Matrix = [[Bool]]
type Index = (Int,Int)
type IMatrix = [[(Index,Bool)]]

generate :: Int -> Int -> Matrix
generate n m =
  let falses = repeat False
      oneTrue = replicate m False ++ [ True ] ++ falses
  in replicate n falses ++ [oneTrue] ++ repeat falses

mt1 = generate 5 3
mt2 = generate 5 7

searchFinite :: IMatrix -> Maybe Index
searchFinite m = let res = concat . map ( dropWhile (\(_,b)->b/=True) ) $ m
                     in case res of
                         (ind,_):t -> Just ind
                         _ -> Nothing

addIndex :: Matrix -> IMatrix
addIndex m = let i = [[(x,y) | y <- [0..]] | x <- [0..]]
                 in zipWith (\ri rm -> zipWith (\ei em -> (ei,em)) ri rm) i m

findTrue :: Matrix -> Index
findTrue m = let im = addIndex m
                 in findTrueRec im 1

findTrueRec :: IMatrix -> Int -> Index
findTrueRec im k = let cond ((i,j),_) = i+j<k
                       lm = takeWhile (/= []) $ map (takeWhile cond) im
                       rm = map (dropWhile cond) im
                       in case searchFinite lm of
                           Just ind -> ind
                           Nothing -> findTrueRec rm (k+1)

main = do
    --print . take 5 . map (take 5) $ addIndex (generate 1 1)
    print $ findTrue mt1
    print $ findTrue mt2