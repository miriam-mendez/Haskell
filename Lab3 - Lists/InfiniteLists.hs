ones :: [Integer]
ones = repeat 1

nats :: [Integer]
nats = iterate (+ 1) 0

ints :: [Integer]
ints = tail (foldr (++) [] (map (\x -> [x, -x]) nats))

triangulars :: [Integer]
triangulars = [res | x <- [0 ..], let res = x * (x + 1) `div` 2]

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = garbell [2..]
    where 
        garbell (p:xs) = p : garbell [x | x <- xs, x `mod` p /= 0]

-- concatMap(\x -> [length x, head x]) [1]
tartaglia :: [[Integer]]
tartaglia = iterate pascal [1]
    where pascal xs = zipWith (+) (0 : xs) (xs ++ [0])