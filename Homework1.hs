todigitsrev :: Integer -> [Integer]
todigitsrev 0 = []
todigitsrev x = x `mod` 10 : todigitsrev (x `div` 10)

todigits :: Integer -> [Integer]
todigits x = reverse (todigitsrev x)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther (x:y:xs) = x : (y * 2) : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x] = x
sumDigits (x:xs)
 x < 10      = x + sumDigits xs
 otherwise   = sumDigits ((todigits) ++ xs)


validate :: Integer -> Bool
validate xs =((sumDigits (doubleEveryOther (todigits xs))) `mod` 10) == 0
