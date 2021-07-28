eql :: [Int] -> [Int] -> Bool
eql xs ys 
    | length xs /= length ys = False
    | otherwise = and $ zipWith (==) xs ys 

prod :: [Int] -> Int
prod = foldl (*) 1

prodOfEvens :: [Int] -> Int
prodOfEvens xs = prod $ filter even xs

powersOf2 :: [Int]
powersOf2 =  map (2^) [0..]

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = sum $ zipWith (*) xs ys