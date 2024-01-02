{-# OPTIONS_GHC -Wall #-}

type Peg = String
type Move = (Peg, Peg)

{- hanoi numbers initial_storage target_storage temporary_storage -}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n x y z
    | n == 0 = []
    | n == 1 = [(x, y)]
    | otherwise = hanoi (n - 1) x z y ++ [(x, y)] ++ hanoi (n - 1) z y x
