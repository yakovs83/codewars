module StockBroker where
input = "ZNGA 1300 2.66 B, CLH15.NYM 50 56.32 B, OWW 1000 11.623 B, OGG 20 580.1 B"

data Result = Correct {quote::String, 
                       quantity::Int, 
                       price::Double, 
                       status::Char} | Incorrect String deriving (Show)

-parseQuote::String->Result
parseQuote q = let w = words q
                   lengthOk = length w == 4 --checking if number of data items is correct
                   priceOk = 

balanceStatement::String->String
balanceStatement st = st

main = do
    putStrLn $ balanceStatement input