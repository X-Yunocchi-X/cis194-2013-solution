import Data.List

fun1 :: [Integer] -> Integer
-- fun1 = foldr (\x y -> (x - 2) * y) 1 . filter even
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then div x 2 else 3 * x + 1)

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf
  where
    insert a Leaf = Node 0 Leaf a Leaf
    insert a (Node depth left val right)
        | height left < height right = let newLeft = insert a left in Node (height newLeft + 1) newLeft val right
        | otherwise = let newRight = insert a right in Node (height newRight + 1) left val newRight
    height Leaf = -1
    height (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldr (\x y -> if x then not y else y) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> f x : y) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ [1 .. n] \\ sieve
  where
    sieve = map (\(i, j) -> i + j + 2 * i * j) . filter (\(i, j) -> i + j + 2 * i * j <= n) $ cartProd [1 .. n] [1 .. n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]
