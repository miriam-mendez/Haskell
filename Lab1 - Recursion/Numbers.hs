absValue :: Int -> Int                  -- retorna el valor absolut
absValue n = abs n

power :: Int -> Int -> Int              -- retorna x elevat a p
power _ 0 = 1
power x p 
   | even p = y * y
   | otherwise = y*y*x
   where 
       y = power x p_halved
       p_halved = p `div` 2

isPrime :: Int -> Bool                  -- indica si es primer o no
isPrime 1 = False
isPrime n =
    let y = [x | x <- [2 .. n], mod n x == 0]
    in length y == 1

divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = [x | x <- [2 .. n], mod n x == 0]

slowFib :: Int -> Int                   -- slow fibonacci
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n - 2) + slowFib (n - 1)

quickFib :: Int -> Int                  -- quick fibonacci
quickFib  n = last (take (n + 1) (aux 0 1))
    where aux x y = x : aux y (x + y)

