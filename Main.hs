
main = print(fac 20)

fac 0 = 1
fac n = n * fac(n-1)

sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial(n-1)

hailstone :: Integer -> Integer
hailstone n = 3 * n + 1


foo :: Integer -> Integer
foo 1
 "Haskell" > "C++" = 3
 otherwise         = 4
foo n
 n < 0 = 0
 n `mod` 17 == 2 = -43
 otherwise = n + 3
