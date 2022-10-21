{-# LANGUAGE MultiParamTypeClasses #-}

module Tigris.Collision.KDTreeMap where

-- base
import Data.List (partition, foldl')
import Data.Maybe (fromJust)


data KDTreeMap k v = Bin !k v !(KDTreeMap k v) !(KDTreeMap k v)
                   | Tip

class IsPoint p n where
  getX :: p n -> n
  getY :: p n -> n
  flipP :: p n -> p n

empty :: KDTreeMap k v
empty = Tip

singleton :: (k, v) -> KDTreeMap k v
singleton (k, v) = Bin k v Tip Tip

quickselect :: (Ord n, IsPoint k n) => Int -> [k n] -> k n
quickselect _ [] = error "'quickselect' not allowed with `[]`"
quickselect i (k : ks) | i < l = quickselect i ys
                            | i > l = quickselect (i - l - 1) zs
                            | otherwise = k
  where (ys, zs) = partition ((<= (getX k)) . getX) ks
        l = length ys

-- use with 'foldl'' I think
quicksort :: (Ord n, IsPoint k n)
          => k n
          -> ([(k n, (k n, v))], Maybe (k n, v), [(k n, (k n, v))])
          -> (k n, (k n, v))
          -> ([(k n, (k n, v))], Maybe (k n, v), [(k n, (k n, v))])
quicksort median (lt, mkv, gt) (k, kv)
  | kx < mx    = ((flipP k, kv) : lt, mkv,                 gt)
  | kx > mx    = (                lt, mkv, (flipP k, kv) : gt)
  | otherwise  =
    case mkv of
      Just _  -> ((flipP k, kv) : lt, mkv,                 gt)
      Nothing -> (                lt, Just kv,             gt)
  where kx = getX k
        mx = getX median


fromList :: (Ord n, IsPoint k n) => [(k n, v)] -> KDTreeMap (k n) v
fromList [] = Tip
fromList kvs = fromListInternal (zip (fst <$> kvs) kvs)
  where
    fromListInternal :: (Ord n, IsPoint k n) => [(k n, (k n, v))] -> KDTreeMap (k n) v
    fromListInternal [] = Tip
    fromListInternal keysKVS = 
      let keys = fst <$> keysKVS
          n = length keys
          median = quickselect (n `div` 2) keys
          (lt, mMedian, rt) = foldl' (quicksort median) ([], Nothing, []) keysKVS
          (k, v) = fromJust mMedian
      in Bin k v (fromListInternal lt) (fromListInternal rt)
