module Golf where

skips :: [a] -> [[a]]
skips list = [each i list | i <- [1..length list]]
  where
    each n list = [ list !! i | i <- [n-1, n-1+n..length list-1]]

localMaxima :: [Integer] -> [Integer]
localMaxima ( x:y:z:xs )
    | y > x && y > z = y:localMaxima ( y:z:xs )
    | otherwise = localMaxima ( y:z:xs )
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs = unlines (map (line c) [m+1,m..1]) ++ "==========\n0123456789\n"
  where c = count xs
        m = maximum c

count :: [Integer] -> [Int]
count list = [ length ( filter (==i) list ) | i <- [0..9]]

line :: [Int] -> Int -> String
line list line = [if i >= line then '*' else ' ' | i <- list]
