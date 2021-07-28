main = do
    x <- getLine
    putStrLn $ girlBoy x

girlBoy::String -> String
girlBoy name 
            |head name == 'A' = "Hello!"
            |head name == 'a' = "Hello!"
            |otherwise = "Bye!"