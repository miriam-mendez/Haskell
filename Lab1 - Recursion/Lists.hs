myLength :: [Int] -> Int
myLength xs = sum [1 | _ <- xs]

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) = x `max` myMaximum xs

average :: [Int] -> Float
average xs = fromIntegral (sum xs) / fromIntegral (length xs)

buildPalindrome :: [Int] -> [Int]
buildPalindrome xs = reverse xs ++ xs

remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove xs [] = xs
remove (x:xs) ys 
    | x `elem` ys = remove xs ys
    |otherwise = x:remove xs ys

flatten :: [[Int]] -> [Int]
flatten xs = concat xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens xs =
    let 
        odds = [x | x <- xs, odd x]
        evens = [x | x <- xs, even x]
    in (odds, evens)


primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors n = [x | x <- [2 .. n], n `mod` x == 0, isPrime x]
    where 
        isPrime :: Int -> Bool
        isPrime n =
            let y = [x | x <- [2 .. n], mod n x == 0]
            in length y == 1

{---------------- 2nd VERSIONS ----------------------- 
myLength :: [Int] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs
-}

