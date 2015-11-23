module StockBroker where
import Text.Read
import Data.Maybe
import Data.List
input1 = "CLH15.NYM 50 56.32 B, ZNGA 1300 2.66 B, OWW 1000 11.623 B, OGG 20 580.1 B"
input2 = "GOOG 300 542.0 B, AAPL 50 145.0 B, CSCO 250.0 29 B, GOOG 200 580.0 S"

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
                             prOk = elem '.' pr --not quite bulletproof, can miss cases like "50." 
                             stOk = st=="B" || st=="S"
                         in if (isJust qttInt && isJust prDouble && prOk && stOk)
                            then Correct quo (fromJust qttInt) (fromJust prDouble) st
                            else Incorrect str
                     
splitOrder::String->[String]                             
splitOrder "" = []
splitOrder str = let (h,t) = break (==',') str
                 in h:splitOrder (drop 1 t)


foldFun::(Double,Double)->Result->(Double,Double)
foldFun acc (Correct _ qt pr st)
    | st == "B" = (fst acc + (fromIntegral qt)*pr,snd acc)
    | st == "S" = (fst acc, snd acc + (fromIntegral qt)*pr)
    | otherwise = acc --not ideal, probably should throw here or somehow handle this case

addCorrect::[Result]->String
addCorrect cs = let (buy,sell) = foldl foldFun (0.0,0.0) cs
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
    putStrLn $ balanceStatement input1
    putStrLn $ balanceStatement input2