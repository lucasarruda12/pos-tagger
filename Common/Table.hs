module Common.Table where

import qualified Data.Map as Map
import Data.List (maximumBy, unzip)
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

moveToIf :: (Ord a, Ord b) => (a -> a) -> (Int -> Bool) -> FrequencyTable a b -> FrequencyTable a b
moveToIf f pred t'@(Table t) = addMany (fmap (\(a, b) -> (f a, b)) affected) $ deleteMany affected t'
  where
    affected = fmap (\(a, bs) -> (a, fst . head . Map.toList $ bs)) $ filter (pred . count . snd) $ (Map.toList t)
    count = foldr (+) 0
    deleteMany abs (Table t) = Table $ foldl (\acc (a', _) -> Map.delete a' acc) t abs
