module StockBroker where
import Text.Read
import Data.Maybe
import Data.List
input = "CLH15.NYM 50 56.32 B, ZNGA 1300 2.66 B, OWW 1000 11.623 B, OGG 20 580.1 B"

data Result = Correct {quote::String, 
                       quantity::Int, 
                       price::Double, 
                       status::String} | Incorrect String deriving (Show,Eq,Ord)

parseQuote::String->Result
parseQuote str = let lengthOk = (length . words $ str) == 4
                 in if not lengthOk then Incorrect str
                    else let [quo,qtt,pr,st] = words str
                             qttInt = readMaybe qtt::Maybe Int
                             prDouble = readMaybe pr::Maybe Double
                             prOk = case pr of
                                 h:'.':t1:t -> True
                                 _ -> False
                             stOk = st=="B" || st=="S"
                         in if (isJust qttInt && isJust prDouble && prOk && stOk)
                            then Correct quo (fromJust qttInt) (fromJust prDouble) st
                            else Incorrect str
                     
splitOrder::String->[String]                             
splitOrder "" = []
splitOrder str = let (h,t) = break (==',') str
                 in h:splitOrder (drop 1 t)

addCorrect::[Result]->String
addCorrect cs = let (buy,sell) = foldl (\acc (Correct _ qt pr st) -> if (st == "B") 
                                                                        then (fst acc + (fromIntegral qt)*pr,snd acc) 
                                                                        else (fst acc, snd acc + (fromIntegral qt)*pr)) (0.0,0.0) cs
                in "Buy: " ++ (show . round $ buy) ++ " Sell: " ++ (show . round $ sell)

grpFun::Result->Result->Bool
grpFun x y = case (x,y) of
                (Incorrect _, Incorrect _) -> True
                (Correct _ _ _ _, Correct _ _ _ _) -> True
                _ -> False

balanceStatement::String->String
balanceStatement st = let ord = groupBy grpFun $ map parseQuote (splitOrder st)
                      in addCorrect $ head ord
                          

main = do
    putStrLn $ balanceStatement input
    --putStrLn $ head . drop 1 . splitOrder $ input