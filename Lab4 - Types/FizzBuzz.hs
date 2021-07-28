fizzBuzz :: [Either Int String]
fizzBuzz = map f [0..]
    where f x
            | x `mod` 15 == 0 = Right "FizzBuzz"
            | x `mod` 3 == 0 = Right "Fizz"
            | x `mod` 5 == 0 = Right "Buzz"
            | otherwise = Left x
