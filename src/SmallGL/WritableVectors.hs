{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, FlexibleInstances #-}
{-# LANGUAGE CPP, UnboxedTuples, MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MagicHash, UnboxedTuples, PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
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
    , writePoint, writePoints
    ) where

import Data.Foldable (foldlM)
import Data.Coerce (coerce)
import GHC.TypeLits (KnownNat)
--import GHCJS.Marshal
--import GHCJS.Types
import Unsafe.Coerce (unsafeCoerce)

import Control.Monad (void)

import JavaScript.TypedArray
import qualified JavaScript.TypedArray.IO as IO
import Control.Monad.ST (ST)
import qualified Control.Monad.ST as ST
--import JavaScript.TypedArray.ST (STTypedArray, STArrayBuffer, STDataView)
import qualified JavaScript.TypedArray.ST as ST

import GHCJS.WebGL.Types

--import Data.Geometry.Prim.JSNum
import Data.Geometry


-- | Coordinate + normal + texture buffer pack
type Vertex20CNT = (Vector3 GLfloat, Vector3 GLbyte, Vector2 GLushort)

-- | write a point into ArrayBuffer
writePoint :: Int -> Vertex20CNT -> STArrayBuffer s -> ST s ()
writePoint offset ( unpackV3 -> (px,py,pz)
           , unpackV3 -> (nx,ny,nz)
           , unpackV2 -> (tx,ty)) buf = do
    ST.unsafeWriteFloat 0 px dv
    ST.unsafeWriteFloat 4 py dv
    ST.unsafeWriteFloat 8 pz dv
    ST.unsafeWriteInt8 12 nx dv
    ST.unsafeWriteInt8 13 ny dv
    ST.unsafeWriteInt8 14 nz dv
    ST.unsafeWriteInt8 15 0 dv
    ST.unsafeWriteWord16 16 tx dv
    ST.unsafeWriteWord16 18 ty dv
    where dv = unsafeDataView' (offset*20) Nothing buf

-- | write a number of points into ArrayBuffer
writePoints :: Foldable f => Int -> f Vertex20CNT -> STArrayBuffer s -> ST s ()
writePoints start ps buf = void $ foldlM f start ps
    where floatView  = arrayView buf
          word16View = arrayView buf
          int8View   = arrayView buf
          f i ( unpackV3 -> (px,py,pz)
              , unpackV3 -> (nx,ny,nz)
              , unpackV2 -> (tx,ty) ) = do
            ST.setIndex (5*i) px floatView
            ST.setIndex (5*i + 1) py floatView
            ST.setIndex (5*i + 2) pz floatView
            ST.setIndex (20*i + 12) nx int8View
            ST.setIndex (20*i + 13) ny int8View
            ST.setIndex (20*i + 14) nz int8View
            ST.setIndex (20*i + 15) 0 int8View
            ST.setIndex (10*i + 8) tx word16View
            ST.setIndex (10*i + 9) ty word16View
            return (i+1)



--              n1 = indexByteArray# arr (unI# 20 *# i +# unI# 12)
--              n2 = indexByteArray# arr (unI# 20 *# i +# unI# 13)
--              n3 = indexByteArray# arr (unI# 20 *# i +# unI# 14)
--              t1 = indexByteArray# arr (unI# 10 *# i +# unI# 8)
--              t2 = indexByteArray# arr (unI# 10 *# i +# unI# 9)
--instance ImmutableArrayBufferPrim (TypedArray Vertex20CNT) where
--    {-# INLINE fromByteArrayPrim #-}
--    fromByteArrayPrim ba = coerce (fromByteArrayPrim ba :: TypedArray t)
--    {-# INLINE toByteArrayPrim #-}
--    toByteArrayPrim arr = toByteArrayPrim (coerce arr :: TypedArray t)
--
--instance MutableArrayBufferPrim (SomeTypedArray m Vertex20CNT) where
--    {-# INLINE fromMutableByteArrayPrim #-}
--    fromMutableByteArrayPrim mba s = case fromMutableByteArrayPrim mba s of
--        (# s1, arr0 :: SomeTypedArray m t #) -> (# s1, coerce arr0 #)
--    {-# INLINE toMutableByteArrayPrim #-}
--    toMutableByteArrayPrim arr = toMutableByteArrayPrim (coerce arr :: SomeTypedArray m t)
--
--instance ArrayBufferData (SomeTypedArray m Vertex20CNT) where
--    {-# INLINE byteLength #-}
--    byteLength arr = byteLength (coerce arr :: SomeTypedArray m t)
--    {-# INLINE sliceImmutable #-}
--    sliceImmutable i0 Nothing arr = coerce $ sliceImmutable
--        (i0* dim (undefined :: Vector n t))
--        Nothing (coerce arr :: SomeTypedArray m t)
--    sliceImmutable i0 (Just i1) arr = coerce $ sliceImmutable
--        (i0*m) (Just $ i1*m) (coerce arr :: SomeTypedArray m t)
--        where m = i0* dim (undefined :: Vector n t)
--
--instance TypedArrayOperations Vertex20CNT where
--    {-# INLINE typedArray #-}
--    typedArray n = coerce (typedArray $ n * dim (undefined :: Vector n t) :: TypedArray t)
--    {-# INLINE fillNewTypedArray #-}
--    fillNewTypedArray n v = js_fillJSArray (coerce v) n (typedArray n :: TypedArray (Vector n t))
--    {-# INLINE fromList #-}
--    fromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs) ar0
--        where ar0 = typedArray (length vs) :: TypedArray (Vector n t)
--    {-# INLINE fromArray #-}
--    fromArray arr = coerce (fromArray arr :: TypedArray t)
--    {-# INLINE arrayView #-}
--    arrayView buf = f $ arrayView buf
--        where f :: SomeTypedArray m t -> SomeTypedArray m (Vector n t)
--              f = coerce
--    {-# INLINE (!) #-}
--    arr ! i = coerce $ js_indexVecArray arr i (dim (undefined :: Vector n t))
--    {-# INLINE elemSize #-}
--    elemSize _ = elemSize (undefined :: TypedArray t) * dim (undefined :: Vector n t)
--    {-# INLINE indexOf #-}
--    indexOf = undefined
--    {-# INLINE lastIndexOf #-}
--    lastIndexOf = undefined
--
--instance IO.IOTypedArrayOperations Vertex20CNT where
--    {-# INLINE newIOTypedArray #-}
--    newIOTypedArray n = coerce
--        <$> (IO.newIOTypedArray $ n * dim (undefined :: Vector n t) :: IO (IOTypedArray t))
--    {-# INLINE fillNewIOTypedArray #-}
--    fillNewIOTypedArray n v = js_fillJSArray (coerce v) n
--        <$> (IO.newIOTypedArray n :: IO (IOTypedArray (Vector n t)))
--    {-# INLINE newFromList #-}
--    newFromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs)
--        <$> (IO.newIOTypedArray (length vs) :: IO (IOTypedArray (Vector n t)))
--    {-# INLINE newFromArray #-}
--    newFromArray arr = coerce <$> (IO.newFromArray arr :: IO (IOTypedArray t))
--    {-# INLINE index #-}
--    index i arr = return . coerce $ js_indexVecArray arr i (dim (undefined :: Vector n t))
--    {-# INLINE setIndex #-}
--    setIndex i vec arr = js_setVecArray i (coerce vec) arr `seq` return ()
--    {-# INLINE setList #-}
--    setList i vecs arr = js_fillListJSArray i (unsafeCoerce $ seqList vecs) arr `seq` return ()
--    {-# INLINE setArray #-}
--    setArray i ar0 arr = IO.setArray (i* dim (undefined :: Vector n t)) ar0 (f arr)
--        where f :: SomeTypedArray m (Vector n t) -> SomeTypedArray m t
--              f = coerce
--
--instance ST.STTypedArrayOperations Vertex20CNT where
--    {-# INLINE newSTTypedArray #-}
--    newSTTypedArray n = coerce
--        <$> (ST.newSTTypedArray $ n * dim (undefined :: Vector n t) :: ST.ST s (STTypedArray s t))
--    {-# INLINE fillNewSTTypedArray #-}
--    fillNewSTTypedArray n v = js_fillJSArray (coerce v) n
--        <$> (ST.newSTTypedArray n :: ST.ST s (STTypedArray s (Vector n t)))
--    {-# INLINE newFromList #-}
--    newFromList vs = js_fillListJSArray 0 (unsafeCoerce $ seqList vs)
--        <$> (ST.newSTTypedArray (length vs) :: ST.ST  s (STTypedArray s (Vector n t)))
--    {-# INLINE newFromArray #-}
--    newFromArray arr = coerce <$> (ST.newFromArray arr :: ST.ST s (STTypedArray s t))
--    {-# INLINE index #-}
--    index i arr = return . coerce $ js_indexVecArray arr i (dim (undefined :: Vector n t))
--    {-# INLINE setIndex #-}
--    setIndex i vec arr = js_setVecArray i (coerce vec) arr `seq` return ()
--    {-# INLINE setList #-}
--    setList i vecs arr = js_fillListJSArray i (unsafeCoerce $ seqList vecs) arr `seq` return ()
--    {-# INLINE setArray #-}
--    setArray i ar0 arr = ST.setArray (i* dim (undefined :: Vector n t)) ar0 (f arr)
--        where f :: SomeTypedArray m (Vector n t) -> SomeTypedArray m t
--              f = coerce




--import GHCJS.WebGL
--import Data.Primitive
--import Data.Traversable
--import Data.Foldable (foldl')
--
--import qualified Foreign.Storable as S
--import GHC.Exts
--import qualified GHC.TypeLits as TL
--
--import Geometry.Space
--
--
--
--
--instance ( Prim a
--         , S.Storable a
--         , S.Storable (Tensor n m a)
--         , Traversable (Tensor n m)
--         , Applicative (Tensor n m)
--         , TL.KnownNat (n TL.* m)
--         )
--         => Prim (Tensor n m a) where
--    sizeOf# x = unI# (numElems x) *# sizeOf# (innerType x)
--    alignment# x = alignment# (innerType x)
--    indexByteArray# arr i0 = snd $
--        mapAccumL (\i _ -> (i + 1, indexByteArray# arr (unI# i)))
--                  (I# i0 * numElems (undefined :: Tensor n m a))
--                  (pure ())
--    readByteArray# arr i0 s0 = (# s1, v #)
--        where !((SB# s1, _ ), v) =
--                  mapAccumL (\(SB# s,i@(I# ii)) _ -> let (# s', x :: a #) = readByteArray# arr ii s
--                                                       in ((SB# s', i+1), x))
--                  (SB# s0, I# i0 * numElems (undefined :: Tensor n m a) )
--                  (pure () :: Tensor n m ())
--    writeByteArray# arr i0' v s0 = s1
--        where i0 = i0' *# unI# (numElems (undefined :: Tensor n m a))
--              !(SIB# (# s1, _ #)) = foldl' (\(SIB# (# s, i #)) x -> SIB# (# writeByteArray# arr i x s, i +# unI# 1 #)) (SIB# (# s0,i0 #)) v
--    setByteArray# arr i0' n v s0 = s1
--        where f i s | i < n' = f (i+1) (writ s)
--                    | otherwise = s
--              !(SIB# (# s1, _ #)) = f 0 (SIB# (# s0,i00 #))
--              n' = I# n
--              i00 = i0' *# unI# (numElems (undefined :: Tensor n m a))
--              writ st = foldl' (\(SIB# (# s, i #)) x -> SIB# (# writeByteArray# arr i x s, i +# unI# 1 #)) st v
--    indexOffAddr# arr i0 = snd $
--        mapAccumL (\i _ -> (i + 1, indexOffAddr# arr (unI# i)))
--                  (I# i0 * numElems (undefined :: Tensor n m a))
--                  (pure ())
--    readOffAddr# arr i0 s0 = (# s1, v #)
--        where !((SB# s1, _ ), v) =
--                  mapAccumL (\(SB# s,i@(I# ii)) _ -> let (# s', x :: a #) = readOffAddr# arr ii s
--                                                       in ((SB# s', i+1), x))
--                  (SB# s0, I# i0 * numElems (undefined :: Tensor n m a) )
--                  (pure () :: Tensor n m ())
--    writeOffAddr# arr i0' v s0 = s1
--        where i0 = i0' *# unI# (numElems (undefined :: Tensor n m a))
--              !(SIB# (# s1, _ #)) = foldl' (\(SIB# (# s, i #)) x -> SIB# (# writeOffAddr# arr i x s, i +# unI# 1 #)) (SIB# (# s0,i0 #)) v
--    setOffAddr# arr i0' n v s0 = s1
--        where f i s | i < n' = f (i+1) (writ s)
--                    | otherwise = s
--              !(SIB# (# s1, _ #)) = f 0 (SIB# (# s0,i00 #))
--              n' = I# n
--              i00 = i0' *# unI# (numElems (undefined :: Tensor n m a))
--              writ st = foldl' (\(SIB# (# s, i #)) x -> SIB# (# writeOffAddr# arr i x s, i +# unI# 1 #)) st v
--


--instance Prim (Vector3 GLfloat, Vector3 GLbyte, Vector2 GLushort) where
--    sizeOf# _ = unI# 20
--    alignment# _ = alignment# (undefined :: GLfloat)
--    indexByteArray# arr i = (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2)
--        where c1 = indexByteArray# arr (unI# 5  *# i)
--              c2 = indexByteArray# arr (unI# 5  *# i +# unI# 1)
--              c3 = indexByteArray# arr (unI# 5  *# i +# unI# 2)
--              n1 = indexByteArray# arr (unI# 20 *# i +# unI# 12)
--              n2 = indexByteArray# arr (unI# 20 *# i +# unI# 13)
--              n3 = indexByteArray# arr (unI# 20 *# i +# unI# 14)
--              t1 = indexByteArray# arr (unI# 10 *# i +# unI# 8)
--              t2 = indexByteArray# arr (unI# 10 *# i +# unI# 9)
--    readByteArray# arr i s0 = (# s8, (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2) #)
--        where (# s1 , c1 #) = readByteArray# arr (unI# 5  *# i) s0
--              (# s2 , c2 #) = readByteArray# arr (unI# 5  *# i +# unI# 1) s1
--              (# s3 , c3 #) = readByteArray# arr (unI# 5  *# i +# unI# 2) s2
--              (# s4 , n1 #) = readByteArray# arr (unI# 20 *# i +# unI# 12) s3
--              (# s5 , n2 #) = readByteArray# arr (unI# 20 *# i +# unI# 13) s4
--              (# s6 , n3 #) = readByteArray# arr (unI# 20 *# i +# unI# 14) s5
--              (# s7 , t1 #) = readByteArray# arr (unI# 10 *# i +# unI# 8) s6
--              (# s8 , t2 #) = readByteArray# arr (unI# 10 *# i +# unI# 9) s7
--    writeByteArray# arr i (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2) s0 = s8
--        where s1 = writeByteArray# arr (unI# 5  *# i) c1 s0
--              s2 = writeByteArray# arr (unI# 5  *# i +# unI# 1) c2 s1
--              s3 = writeByteArray# arr (unI# 5  *# i +# unI# 2) c3 s2
--              s4 = writeByteArray# arr (unI# 20 *# i +# unI# 12) n1 s3
--              s5 = writeByteArray# arr (unI# 20 *# i +# unI# 13) n2 s4
--              s6 = writeByteArray# arr (unI# 20 *# i +# unI# 14) n3 s5
--              s7 = writeByteArray# arr (unI# 10 *# i +# unI# 8) t1 s6
--              s8 = writeByteArray# arr (unI# 10 *# i +# unI# 9) t2 s7
--    setByteArray# arr i0 n v = f (I# i0)
--        where f i s | i < n' = f (i+1) (writeByteArray# arr (unI# i) v s)
--                    | otherwise = s
--              n' = I# (n +# i0)
--    indexOffAddr# arr i = (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2)
--        where c1 = indexOffAddr# arr (unI# 5  *# i)
--              c2 = indexOffAddr# arr (unI# 5  *# i +# unI# 1)
--              c3 = indexOffAddr# arr (unI# 5  *# i +# unI# 2)
--              n1 = indexOffAddr# arr (unI# 20 *# i +# unI# 12)
--              n2 = indexOffAddr# arr (unI# 20 *# i +# unI# 13)
--              n3 = indexOffAddr# arr (unI# 20 *# i +# unI# 14)
--              t1 = indexOffAddr# arr (unI# 10 *# i +# unI# 8)
--              t2 = indexOffAddr# arr (unI# 10 *# i +# unI# 9)
--    readOffAddr# arr i s0 = (# s8, (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2) #)
--        where (# s1 , c1 #) = readOffAddr# arr (unI# 5  *# i) s0
--              (# s2 , c2 #) = readOffAddr# arr (unI# 5  *# i +# unI# 1) s1
--              (# s3 , c3 #) = readOffAddr# arr (unI# 5  *# i +# unI# 2) s2
--              (# s4 , n1 #) = readOffAddr# arr (unI# 20 *# i +# unI# 12) s3
--              (# s5 , n2 #) = readOffAddr# arr (unI# 20 *# i +# unI# 13) s4
--              (# s6 , n3 #) = readOffAddr# arr (unI# 20 *# i +# unI# 14) s5
--              (# s7 , t1 #) = readOffAddr# arr (unI# 10 *# i +# unI# 8) s6
--              (# s8 , t2 #) = readOffAddr# arr (unI# 10 *# i +# unI# 9) s7
--    writeOffAddr# arr i (Vector3 c1 c2 c3, Vector3 n1 n2 n3, Vector2 t1 t2) s0 = s8
--        where s1 = writeOffAddr# arr (unI# 5  *# i) c1 s0
--              s2 = writeOffAddr# arr (unI# 5  *# i +# unI# 1) c2 s1
--              s3 = writeOffAddr# arr (unI# 5  *# i +# unI# 2) c3 s2
--              s4 = writeOffAddr# arr (unI# 20 *# i +# unI# 12) n1 s3
--              s5 = writeOffAddr# arr (unI# 20 *# i +# unI# 13) n2 s4
--              s6 = writeOffAddr# arr (unI# 20 *# i +# unI# 14) n3 s5
--              s7 = writeOffAddr# arr (unI# 10 *# i +# unI# 8) t1 s6
--              s8 = writeOffAddr# arr (unI# 10 *# i +# unI# 9) t2 s7
--    setOffAddr# arr i0 n v = f (I# i0)
--        where f i s | i < n' = f (i+1) (writeOffAddr# arr (unI# i) v s)
--                    | otherwise = s
--              n' = I# (n +# i0)
--
--
--
--
--innerType :: Tensor n m a -> a
--innerType _ = undefined
--
--
--unI# :: Int -> Int#
--unI# (I# n#) = n#
--
--
--data StateBox s = SB# (State# s)
--data StateIBox s = SIB# (# State# s, Int# #)








