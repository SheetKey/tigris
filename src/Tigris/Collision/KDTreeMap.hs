{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Tigris.Collision.KDTreeMap where

-- base
import Data.List (partition, foldl')
import Data.Maybe (fromJust)
import GHC.Generics

-- deepseq
import Control.DeepSeq


data KDTreeMap k v = Bin !k v !(KDTreeMap k v) !(KDTreeMap k v)
                   | Tip
                   deriving Generic

instance (NFData k, NFData v) => NFData (KDTreeMap k v)

class (Ord n, Num n) => IsPoint p n where
  getX :: p n -> n
  getY :: p n -> n
  flipP :: p n -> p n
  distSqr :: p n -> p n -> n

instance (Ord n, Num n) => IsPoint ((,) n) n where
  getX = fst
  getY = snd
  flipP (x, y) = (y, x)
  distSqr (x1, y1) (x2, y2) = let x = x1 - x2
                                  y = y1 - y2
                              in (x * x) + (y * y)

empty :: KDTreeMap k v
empty = Tip

singleton :: (k, v) -> KDTreeMap k v
singleton (k, v) = Bin k v Tip Tip

quickselect :: IsPoint k n => Int -> [k n] -> k n
quickselect _ [] = error "'quickselect' not allowed with `[]`"
quickselect _ (k : []) = k
quickselect i (k : ks) | i < l = quickselect i ys
                       | i > l = quickselect (i - l - 1) zs
                       | otherwise = k
  where (ys, zs) = partition ((<= (getX k)) . getX) ks
        l = length ys

-- use with 'foldl'' I think
quicksort :: IsPoint k n
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

quicksort2 :: IsPoint k n
           => k n
           -> Int
           -> ([(k n, v)], Maybe (k n, v), [(k n, v)])
           -> (k n, v)
           -> ([(k n, v)], Maybe (k n, v), [(k n, v)])
quicksort2 m index acc kvi = go m index acc kvi
  where
    go median idx (lt, mkv, gt) kv@(k, _) =
      if idx == 0
      then let kx = getX k
               mx = getX median
           in case kx `compare` mx of
                LT -> (kv : lt, mkv,      gt)
                GT -> (     lt, mkv, kv : gt)
                EQ -> case mkv of
                        Just _  -> (kv : lt, mkv,     gt)
                        Nothing -> (     lt, Just kv, gt)
      else let ky = getY k
               my = getY median
           in case ky `compare` my of
                LT -> (kv : lt, mkv,      gt)
                GT -> (     lt, mkv, kv : gt)
                EQ -> case mkv of
                        Just _  -> (kv : lt, mkv,     gt)
                        Nothing -> (     lt, Just kv, gt)

fromList2 :: IsPoint k n => [(k n, v)] -> KDTreeMap (k n) v
fromList2 [] = Tip
fromList2 kvS = go kvS 0
  where
    go []  _   = Tip
    go kvs idx = 
      let n = length kvs
          median = quickselect (n `div` 2) (fst <$> kvs)
          (lt, mMedian, gt) = foldl' (quicksort2 median idx) ([], Nothing, []) kvs
          (k, v) = fromJust mMedian
      in if idx == 0
         then Bin k v (go lt 1) (go gt 1)
         else Bin k v (go lt 0) (go gt 0)


fromList :: IsPoint k n => [(k n, v)] -> KDTreeMap (k n) v
fromList [] = Tip
fromList kvs = fromListInternal (zip (fst <$> kvs) kvs)
  where
    fromListInternal :: IsPoint k n => [(k n, (k n, v))] -> KDTreeMap (k n) v
    fromListInternal [] = Tip
    fromListInternal keysKVS = 
      let keys = fst <$> keysKVS
          n = length keys
          median = quickselect (n `div` 2) keys
          (lt, mMedian, rt) = foldl' (quicksort median) ([], Nothing, []) keysKVS
          (k, v) = fromJust mMedian
      in Bin k v (fromListInternal lt) (fromListInternal rt)

fromList2r :: IsPoint k n => [(k n, v)] -> KDTreeMap (k n) v
fromList2r [] = Tip
fromList2r kvS = go kvS 0
  where
    go []  _   = Tip
    go kvs idx = 
      let n = length kvs
          median = quickselect (n `div` 2) (fst <$> kvs)
          (lt, mMedian, gt) = foldr (flip $ quicksort2 median idx) ([], Nothing, []) kvs
          (k, v) = fromJust mMedian
      in if idx == 0
         then Bin k v (go lt 1) (go gt 1)
         else Bin k v (go lt 0) (go gt 0)


fromListr :: IsPoint k n => [(k n, v)] -> KDTreeMap (k n) v
fromListr [] = Tip
fromListr kvs = fromListInternal (zip (fst <$> kvs) kvs)
  where
    fromListInternal :: IsPoint k n => [(k n, (k n, v))] -> KDTreeMap (k n) v
    fromListInternal [] = Tip
    fromListInternal keysKVS = 
      let keys = fst <$> keysKVS
          n = length keys
          median = quickselect (n `div` 2) keys
          (lt, mMedian, rt) = foldr (flip $ quicksort median) ([], Nothing, []) keysKVS
          (k, v) = fromJust mMedian
      in Bin k v (fromListInternal lt) (fromListInternal rt)

inRadius :: IsPoint k n => n -> k n -> KDTreeMap (k n) v -> [(k n, v)]
inRadius radius point tree = go 0 radius point tree []
  where
    go :: IsPoint k n => Int -> n -> k n -> KDTreeMap (k n) v -> [(k n, v)] -> [(k n, v)]
    go _ _ _ Tip acc = acc
    go idx r p (Bin k v left right) acc =
      if idx == 0
      then let onLeft = getX p <= getX k
               accOnSide = if onLeft
                           then go 1 r p left acc
                           else go 1 r p right acc
               accOffSide = if abs (getX p - getX k) < r
                            then if onLeft
                                 then go 1 r p right accOnSide
                                 else go 1 r p left accOnSide
                            else accOnSide
               finalAcc = if distSqr k p <= r * r
                          then (k, v) : accOffSide
                          else accOffSide
           in finalAcc
      else let onLeft = getY p <= getY k
               accOnSide = if onLeft
                           then go 0 r p left acc
                           else go 0 r p right acc
               accOffSide = if abs (getY p - getY k) < r
                            then if onLeft
                                 then go 0 r p right accOnSide
                                 else go 0 r p left accOnSide
                            else accOnSide
               finalAcc = if distSqr k p <= r * r
                          then (k, v) : accOffSide
                          else accOffSide
           in finalAcc
