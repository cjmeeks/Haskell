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
 | x < 10      = x + sumDigits xs
 | otherwise   = sumDigits ((todigits x) ++ xs)


validate :: Integer -> Bool
validate xs = (sumDigits (doubleEveryOther (todigitsrev xs))) `mod` 10 == 0

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 a b c = []
hanoi 1 a b c = (a, c) : []
hanoi n a b c = (a, b) : (a, c) : (b, c) : (hanoiRec (n-2) a b c)

hanoiRec :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoiRec 0 a b c = []
hanoiRec 1 a b c = (a, b) : (c, b) : (b, c) : []
hanoiRec n a b c = (a, b) : (c, b) : (a, c) : (b, c) : hanoiRec (n-2) a b c
