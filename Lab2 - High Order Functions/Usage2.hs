flatten :: [[Int]] -> [Int]
flatten = foldl (\acc xs -> acc ++ xs ) []
 
myLength :: String -> Int
myLength = foldl (\acc _ -> 1 + acc) 0

myReverse :: [Int] -> [Int]
myReverse = foldl (\acc x -> x : acc) []

countIn :: [[Int]] -> Int -> [Int]
countIn xs n = map (length . filter (== n)) xs

firstWord :: String -> String
firstWord = takeWhile (/= ' ') . dropWhile (==' ')