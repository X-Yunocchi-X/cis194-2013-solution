{-# OPTIONS_GHC -Wall #-}

module Credit where

toDigitRev :: Integer -> [Integer]
toDigitRev n
    | n <= 0 = []
    | otherwise = mod n 10 : toDigitRev (div n 10)

toDigit :: Integer -> [Integer]
toDigit n
    | n <= 0 = []
    | otherwise = toDigit (div n 10) ++ [mod n 10]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherHelper . reverse
  where
    doubleEveryOtherHelper [] = []
    doubleEveryOtherHelper [x] = [x]
    doubleEveryOtherHelper (x : y : xs) = x : 2 * y : doubleEveryOtherHelper xs

sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+) . sum . toDigit) 0

validate :: Integer -> Bool
validate n = mod ((sumDigits . doubleEveryOther . toDigit) n) 10 == 0
