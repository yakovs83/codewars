module StockBroker where
import Text.Read
import Data.Maybe
input = "ZNGA 1300 2.66 B, CLH15.NYM 50 56.32 B, OWW 1000 11.623 B, OGG 20 580.1 B"

data Result = Correct {quote::String, 
                       quantity::Int, 
                       price::Double, 
                       status::String} | Incorrect String deriving (Show)

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
                   

balanceStatement::String->String
balanceStatement st = st

main = do
    --putStrLn $ balanceStatement input
    putStrLn $ head . drop 1 . splitOrder $ input