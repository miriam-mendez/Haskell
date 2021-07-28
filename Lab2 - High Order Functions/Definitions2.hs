countIf :: (Int -> Bool) -> [Int] -> Int
countIf p xs = length $ filter p xs

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam xs fs = map (\f -> map f xs) fs

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 xs fs = map (\x -> map (\f -> f x) fs) xs

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int 
filterFoldl p f x = foldr f x . filter p

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert _ [] x = [x]
insert f all@(x:xs) y 
    |f x y = x:insert f xs y
    |otherwise = y:all

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort p xs = foldr (\x xs -> insert p xs x) [] xs