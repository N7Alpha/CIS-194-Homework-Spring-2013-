{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}

module Risk where

import Control.Monad.Random
import Data.List

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

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle b = replicateM atk die >>=
            \aDice -> replicateM def die >>=
            \dDice -> 
              let cmp f = length $ filter (== True) $ zipWith f (sort aDice) (sort dDice) in
              return (Battlefield (attackers b - cmp (>)) (defenders b - cmp (<=)))    
      where atk = min 3 $ attackers b - 1
            def = min 2 $ defenders b

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield atk def)
                | atk > 1 && def > 0 = battle b >>= invade
                | otherwise = return b


successProb :: Battlefield -> Rand StdGen Double
successProb b = replicateM 1000 (invade b) >>=
                \ battles -> return (fromIntegral (length (filter ((== 0) . defenders) battles)) / 1000.0)

-- Exercise 5

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined