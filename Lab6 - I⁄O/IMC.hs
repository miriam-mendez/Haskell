bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi < skinny = "underweight"  
    | bmi < normal = "normal weight"  
    | bmi < fat    = "overweight"  
    | bmi < obese  = "obese"  
    | otherwise    = "severely obese"  
    where bmi = weight / height ^ 2  
          (skinny, normal, fat, obese) = (18, 25, 30, 40)    

main = do
    line <- getLine
    let xs = words line
    if length xs == 1
      then return ()
      else do
        let name   = xs !! 0
        let weight = read $ xs !! 1
        let height = read $ xs !! 2
        putStrLn $ name ++ ": " ++ bmiTell weight height
        main