myMap::(a -> b) -> [a] -> [b]
myMap f xs = [ res | x <- xs, let res = f x] 

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [x | x <- xs, p x]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [res | (x, y) <- zip xs ys , let res = f x y]

thingify :: [Int] -> [Int] -> [(Int,Int)]
thingify xs ys = [res | x <- xs, y <- ys, let res = (x,y), mod x y == 0]

factors :: Int -> [Int]
factors x = [y | y <- [1 .. x], mod x y == 0]