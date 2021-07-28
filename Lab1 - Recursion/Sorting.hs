insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert all@(x:xs) n 
    | x > n = n:all
    | otherwise = x:insert xs n

isort :: [Int] -> [Int]
isort [] = []
isort [x] = [x]
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int] -- s'assumenix que l'element n sempre està a la llista
remove (x:xs) n 
    | n == x = xs
    |otherwise = x:remove xs n

ssort :: [Int] -> [Int]
ssort [] = []
ssort xs =
    let smallerValue = minimum xs
    in smallerValue:ssort (remove xs smallerValue)

merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys) 
    | x < y =  x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = 
    let (as,bs) = splitAt xs_halved xs
        xs_halved = length xs `div` 2
    in merge (msort as) (msort bs)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = 
    let smallerSorted = qsort [a | a <- xs, a <= x]
        biggerSorted = qsort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:xs) = 
    let smallerSorted = genQsort [a | a <- xs, a <= x]
        biggerSorted = genQsort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted