module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a next) = a : streamToList next

instance (Show a) => Show (Stream a) where
    show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a $ streamRepeat a

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a next) = Cons (f a) $ streamMap f next

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed $ streamFromSeed f (f seed)

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = rulerStart 0
  where
    rulerStart x = interleaveStreams (streamRepeat x) $ rulerStart (x + 1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a an) b = Cons a $ interleaveStreams b an
