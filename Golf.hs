module Golf where

import Data.List


every :: [a] -> Int -> [a]
every n xs = case drop (n - 1) xs of
             (y:ys) -> y : every n ys
             [] -> []

skips :: [a] -> [[a]]
skips [] = []
skips x@(_:_) = map (every x) [0..length (x - 1)]


localMaxima :: [Integer] -> [Integer]
localMaxima (a:x:b:xs)
  | a < x && b < x = [x] ++ localMaxima (b:xs)
  | otherwise      = localMaxima (x:b:xs)
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

line :: [Int] -> Int -> String
line xs n = [if i >= n then '*' else ' ' | i <- xs]

count :: [Integer] -> [Int]
count xs = map (\n -> length ( filter (== n) xs )) [0..9]
