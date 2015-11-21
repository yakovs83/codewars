module StockBroker where
input = "ZNGA 1300 2.66 B, CLH15.NYM 50 56.32 B, OWW 1000 11.623 B, OGG 20 580.1 B"

balanceStatement::String->String
balanceStatement st = st

main = do
    putStrLn $ balanceStatement input