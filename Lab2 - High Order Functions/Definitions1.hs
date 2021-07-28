myFoldl :: (a -> b -> a) -> a -> [b] -> a -- recursiva
myFoldl f x [] = x
myFoldl f x (y:ys) =  myFoldl f (f x y) ys

myFoldr :: (a -> b -> b) -> b -> [a] -> b -- recursiva
myFoldr f x [] = x
myFoldr f x (y:ys) =  f y (myFoldr f x ys)

myIterate :: (a -> a) -> a -> [a] -- recursiva
myIterate f x = x:myIterate f (f x)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a -- recursiva
myUntil p f x 
    | p x = x
    |otherwise = myUntil p f (f x)

myMap :: (a -> b) -> [a] -> [b]
myMap f xs = foldr (\x acc -> f x : acc) [] xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\x acc -> if p x then x : acc else acc) []

myAll :: (a -> Bool) -> [a] -> Bool
myAll f = foldr (\a b -> f a && b) True

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> f a || b) False

myZip :: [a] -> [b] -> [(a, b)] -- recursiva
myZip _ [] = []
myZip [] _ = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = map(\ x y -> f x y) . zip xs ys