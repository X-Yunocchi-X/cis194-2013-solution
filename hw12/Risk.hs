{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: (Int, Int) -> Rand StdGen ([DieValue], [DieValue])
dice (atk, dfd) = (,) <$> replicateM atk die <*> replicateM dfd die

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

compareDice :: ([DieValue], [DieValue]) -> (Army, Army) -> (Army, Army)
compareDice ([], _) troops = troops
compareDice (atkv, []) (atka, dfda) = (atka, dfda - length atkv)
compareDice (av:avs, dv:dvs) (atka, dfda)
  | av > dv = compareDice (avs, dvs) (atka, dfda - 1)
  | otherwise = compareDice (avs, dvs) (atka - 1, dfda)

afterBattle :: (Army, Army) -> Battlefield
afterBattle = uncurry Battlefield

getTroops :: Battlefield -> (Army, Army)
getTroops (Battlefield atk dfd) = (atroop, dtroop)
  where atroop = if atk >= 4 then 3 else atk-1
        dtroop = min dfd 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield atk dfd) = dice (getTroops bf) >>= (\dies -> battle $ afterBattle (compareDice dies (atk, dfd)))

invade :: Battlefield -> Rand StdGen Battlefield
invade bf = battle bf >>= (\af@(Battlefield atk dfd) -> if atk < 2 || dfd == 0 then battle af else return af)

success :: [Battlefield] -> Rand StdGen Double
success bfs = return $ fromIntegral (length x) / fromIntegral (length bfs)
  where x = filter ((== 0) . defenders) bfs

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= success