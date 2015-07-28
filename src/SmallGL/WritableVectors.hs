{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE CPP, UnboxedTuples, MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SmallGL.WritableVectors
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
--
--
--
-----------------------------------------------------------------------------

module SmallGL.WritableVectors
    ( Vertex20CNT
    ) where

import GHCJS.WebGL
import Data.Primitive
import Data.Traversable
import Data.Foldable (foldl')

import qualified Foreign.Storable as S
import GHC.Exts
import qualified GHC.TypeLits as TL

import Geometry.Space




instance ( Prim a
         , S.Storable a
         , S.Storable (Tensor n m a)
         , Traversable (Tensor n m)
         , Applicative (Tensor n m)
         , TL.KnownNat (n TL.* m)
         )
         => Prim (Tensor n m a) where
    sizeOf# x = unI# (numElems x) *# sizeOf# (innerType x)
    alignment# x = alignment# (innerType x)
    indexByteArray# arr i0 = snd $
        mapAccumL (\i _ -> (i + 1, indexByteArray# arr (unI# i)))
                  (I# i0 * numElems (undefined :: Tensor n m a))
                  (pure ())
    readByteArray# arr i0 s0 = (# s1, v #)
        where !((SB# s1, _ ), v) =
                  mapAccumL (\(SB# s,i@(I# ii)) _ -> let (# s', x :: a #) = readByteArray# arr ii s
                                                       in ((SB# s', i+1), x))
                  (SB# s0, I# i0 * numElems (undefined :: Tensor n m a) )
                  (pure () :: Tensor n m ())
    writeByteArray# arr i0' v s0 = s1
        where i0 = i0' *# unI# (numElems (undefined :: Tensor n m a))
              !(SIB# (# s1, _ #)) = foldl' (\(SIB# (# s, i #)) x -> SIB# (# writeByteArray# arr i x s, i +# unI# 1 #)) (SIB# (# s0,i0 #)) v
    setByteArray# arr i0' n v s0 = s1
        where f i s | i < n' = f (i+1) (writ s)
                    | otherwise = s
              !(SIB# (# s1, _ #)) = f 0 (SIB# (# s0,i00 #))
              n' = I# n
              i00 = i0' *# unI# (numElems (undefined :: Tensor n m a))
              writ st = foldl' (\(SIB# (# s, i #)) x -> SIB# (# writeByteArray# arr i x s, i +# unI# 1 #)) st v
    indexOffAddr# arr i0 = snd $
        mapAccumL (\i _ -> (i + 1, indexOffAddr# arr (unI# i)))
                  (I# i0 * numElems (undefined :: Tensor n m a))
                  (pure ())
    readOffAddr# arr i0 s0 = (# s1, v #)
        where !((SB# s1, _ ), v) =
                  mapAccumL (\(SB# s,i@(I# ii)) _ -> let (# s', x :: a #) = readOffAddr# arr ii s
                                                       in ((SB# s', i+1), x))
                  (SB# s0, I# i0 * numElems (undefined :: Tensor n m a) )
                  (pure () :: Tensor n m ())
    writeOffAddr# arr i0' v s0 = s1
        where i0 = i0' *# unI# (numElems (undefined :: Tensor n m a))
              !(SIB# (# s1, _ #)) = foldl' (\(SIB# (# s, i #)) x -> SIB# (# writeOffAddr# arr i x s, i +# unI# 1 #)) (SIB# (# s0,i0 #)) v
    setOffAddr# arr i0' n v s0 = s1
        where f i s | i < n' = f (i+1) (writ s)
                    | otherwise = s
              !(SIB# (# s1, _ #)) = f 0 (SIB# (# s0,i00 #))
              n' = I# n
              i00 = i0' *# unI# (numElems (undefined :: Tensor n m a))
              writ st = foldl' (\(SIB# (# s, i #)) x -> SIB# (# writeOffAddr# arr i x s, i +# unI# 1 #)) st v


-- | Coordinate + normal + texture buffer pack
type Vertex20CNT = (Vector3 GLfloat, Vector3 GLbyte, Vector2 GLushort)

instance Prim (Vector3 GLfloat, Vector3 GLbyte, Vector2 GLushort) where
    sizeOf# _ = unI# 20
    alignment# _ = alignment# (undefined :: GLfloat)
    indexByteArray# arr i = (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2)
        where c1 = indexByteArray# arr (unI# 5  *# i)
              c2 = indexByteArray# arr (unI# 5  *# i +# unI# 1)
              c3 = indexByteArray# arr (unI# 5  *# i +# unI# 2)
              n1 = indexByteArray# arr (unI# 20 *# i +# unI# 12)
              n2 = indexByteArray# arr (unI# 20 *# i +# unI# 13)
              n3 = indexByteArray# arr (unI# 20 *# i +# unI# 14)
              t1 = indexByteArray# arr (unI# 10 *# i +# unI# 8)
              t2 = indexByteArray# arr (unI# 10 *# i +# unI# 9)
    readByteArray# arr i s0 = (# s8, (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2) #)
        where (# s1 , c1 #) = readByteArray# arr (unI# 5  *# i) s0
              (# s2 , c2 #) = readByteArray# arr (unI# 5  *# i +# unI# 1) s1
              (# s3 , c3 #) = readByteArray# arr (unI# 5  *# i +# unI# 2) s2
              (# s4 , n1 #) = readByteArray# arr (unI# 20 *# i +# unI# 12) s3
              (# s5 , n2 #) = readByteArray# arr (unI# 20 *# i +# unI# 13) s4
              (# s6 , n3 #) = readByteArray# arr (unI# 20 *# i +# unI# 14) s5
              (# s7 , t1 #) = readByteArray# arr (unI# 10 *# i +# unI# 8) s6
              (# s8 , t2 #) = readByteArray# arr (unI# 10 *# i +# unI# 9) s7
    writeByteArray# arr i (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2) s0 = s8
        where s1 = writeByteArray# arr (unI# 5  *# i) c1 s0
              s2 = writeByteArray# arr (unI# 5  *# i +# unI# 1) c2 s1
              s3 = writeByteArray# arr (unI# 5  *# i +# unI# 2) c3 s2
              s4 = writeByteArray# arr (unI# 20 *# i +# unI# 12) n1 s3
              s5 = writeByteArray# arr (unI# 20 *# i +# unI# 13) n2 s4
              s6 = writeByteArray# arr (unI# 20 *# i +# unI# 14) n3 s5
              s7 = writeByteArray# arr (unI# 10 *# i +# unI# 8) t1 s6
              s8 = writeByteArray# arr (unI# 10 *# i +# unI# 9) t2 s7
    setByteArray# arr i0 n v = f (I# i0)
        where f i s | i < n' = f (i+1) (writeByteArray# arr (unI# i) v s)
                    | otherwise = s
              n' = I# (n +# i0)
    indexOffAddr# arr i = (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2)
        where c1 = indexOffAddr# arr (unI# 5  *# i)
              c2 = indexOffAddr# arr (unI# 5  *# i +# unI# 1)
              c3 = indexOffAddr# arr (unI# 5  *# i +# unI# 2)
              n1 = indexOffAddr# arr (unI# 20 *# i +# unI# 12)
              n2 = indexOffAddr# arr (unI# 20 *# i +# unI# 13)
              n3 = indexOffAddr# arr (unI# 20 *# i +# unI# 14)
              t1 = indexOffAddr# arr (unI# 10 *# i +# unI# 8)
              t2 = indexOffAddr# arr (unI# 10 *# i +# unI# 9)
    readOffAddr# arr i s0 = (# s8, (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2) #)
        where (# s1 , c1 #) = readOffAddr# arr (unI# 5  *# i) s0
              (# s2 , c2 #) = readOffAddr# arr (unI# 5  *# i +# unI# 1) s1
              (# s3 , c3 #) = readOffAddr# arr (unI# 5  *# i +# unI# 2) s2
              (# s4 , n1 #) = readOffAddr# arr (unI# 20 *# i +# unI# 12) s3
              (# s5 , n2 #) = readOffAddr# arr (unI# 20 *# i +# unI# 13) s4
              (# s6 , n3 #) = readOffAddr# arr (unI# 20 *# i +# unI# 14) s5
              (# s7 , t1 #) = readOffAddr# arr (unI# 10 *# i +# unI# 8) s6
              (# s8 , t2 #) = readOffAddr# arr (unI# 10 *# i +# unI# 9) s7
    writeOffAddr# arr i (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2) s0 = s8
        where s1 = writeOffAddr# arr (unI# 5  *# i) c1 s0
              s2 = writeOffAddr# arr (unI# 5  *# i +# unI# 1) c2 s1
              s3 = writeOffAddr# arr (unI# 5  *# i +# unI# 2) c3 s2
              s4 = writeOffAddr# arr (unI# 20 *# i +# unI# 12) n1 s3
              s5 = writeOffAddr# arr (unI# 20 *# i +# unI# 13) n2 s4
              s6 = writeOffAddr# arr (unI# 20 *# i +# unI# 14) n3 s5
              s7 = writeOffAddr# arr (unI# 10 *# i +# unI# 8) t1 s6
              s8 = writeOffAddr# arr (unI# 10 *# i +# unI# 9) t2 s7
    setOffAddr# arr i0 n v = f (I# i0)
        where f i s | i < n' = f (i+1) (writeOffAddr# arr (unI# i) v s)
                    | otherwise = s
              n' = I# (n +# i0)




innerType :: Tensor n m a -> a
innerType _ = undefined


unI# :: Int -> Int#
unI# (I# n#) = n#


data StateBox s = SB# (State# s)
data StateIBox s = SIB# (# State# s, Int# #)








