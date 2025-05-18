module Common.Table where

import qualified Data.Map as Map
import Data.List (maximumBy)
import Data.Ord (comparing)

newtype Table a b c = Table (Map.Map a (Map.Map b c))

instance Functor (Table a b) where
  fmap f (Table t) = Table $ fmap (fmap f) t

type FrequencyTable a b = Table a b Int

emptyft :: FrequencyTable a b
emptyft = Table $ Map.empty

add :: (Ord a, Ord b, Eq a, Eq b) => a -> b -> FrequencyTable a b -> FrequencyTable a b
add a b (Table t) = Table $ Map.insertWith (Map.unionWith (+)) a (Map.singleton b 1) t

addMany :: (Ord a, Ord b, Eq a, Eq b) => [(a, b)] -> FrequencyTable a b -> FrequencyTable a b
addMany xs t = foldr (uncurry add) t xs

prune :: FrequencyTable a b -> [(a, b)]
prune (Table t) = Map.toList $ fmap filterMostFrequent t
  where
    filterMostFrequent :: Map.Map b Int -> b
    filterMostFrequent = fst . maximumBy (comparing snd) . Map.toList
