{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHCJS.Instances
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module GHCJS.Instances () where



--import Unsafe.Coerce
--import GHCJS.Foreign
--import GHCJS.Types
--import GHCJS.Marshal
--import Control.Monad (liftM, join)
--
--import Data.Geometry
--import Geometry.Structure

--import Debug.Trace
--import GHCJS.Useful
--
--instance ToJSVal => ToJSRef (Vector4 a) where
--    toJSVal = mapM toJSVal >>= \(Vector4 a b c d) -> unsafeCoerce (toArray4 a b c d)
--
--instance ToJSVal => ToJSRef (Vector3 a) where
--    toJSVal = mapM toJSVal >>= \(Vector3 a b c) -> unsafeCoerce (toArray3 a b c)
--
--instance  ToJSVal => ToJSRef (Vector2 a) where
--    toJSVal = mapM toJSVal >>= \(Vector2 a b) -> liftM unsafeCoerce (toArray2 a b)
--
--instance (FromJSVal, Num a) => FromJSRef (Vector4 a) where
--    fromJSVal = fromJSRefListOf (unsafeCoerce vref) >>= \marr -> case marr of
--        Just (a:b:c:d:_) -> return . Just $ Vector4 a b c d
--        Just (a:b:c:_) -> return . Just $ Vector4 a b c 0
--        Just (a:b:_) -> return . Just $ Vector4 a b 0 0
--        _ -> return $ Nothing
--
--instance (FromJSVal, Num a) => FromJSRef (Vector3 a) where
--    fromJSVal = fromJSRefListOf (unsafeCoerce vref) >>= \marr -> case marr of
--        Just (a:b:c:_) -> return . Just $ Vector3 a b c
--        Just (a:b:_) -> return . Just $ Vector3 a b 0
--        _ -> return $ Nothing
--
--instance FromJSVal => FromJSRef (Vector2 a) where
--    fromJSVal = fromJSRefListOf (unsafeCoerce vref) >>= \marr -> case marr of
--        Just (a:b:_) -> return . Just $ Vector2 a b
--        _ -> return $ Nothing
--
--
--instance (ToJSRef (Vector n x)) => ToJSRef (Polygon n x) where
--    toJSRef (SimpleConvexPolygon xs) = liftM unsafeCoerce $ mapM toJSVal >>= toArray >>= toArray1
--    toJSRef (SimplePolygon xs) = liftM unsafeCoerce $ mapM toJSVal >>= toArray >>= toArray1
--    toJSRef (GenericPolygon xss) = liftM unsafeCoerce
--        $ mapM (\p -> toJSVal >>= fromArray . unsafeCoerce) xss >>= toArray . join
--
--instance (FromJSRef (Vector n x)) => FromJSRef (Polygon n x) where
--    fromJSVal = fromJSRefListOf (unsafeCoerce x) -- :: JSRef [JSRef [JSRef (Vector n x)]])
--            >>= \ps -> case ps of
--        Nothing  -> return Nothing
--        Just []  -> return Nothing
--        Just [p] -> f p
--        Just pss -> liftM (liftM GenericPolygon . sequenceA) $ mapM f pss
--      where f refarr = fromJSRefListOf refarr >>= \marr -> case marr of
--                        Nothing -> return Nothing
--                        Just rvecs -> liftM (liftM SimplePolygon . sequenceA) $ mapM fromJSVal
--
--
--
--
--foreign import javascript unsafe "$r = [$1, $2, $3, $4]"
--    toArray4 :: JSVal -> JSVal -> JSVal -> JSVal -> IO (JSArray a)
--
--foreign import javascript unsafe "$r = [$1, $2, $3]"
--    toArray3 :: JSVal -> JSVal -> JSVal -> IO (JSArray a)
--
--foreign import javascript unsafe "$r = [$1, $2]"
--    toArray2 :: JSVal -> JSVal -> IO (JSArray a)
--
--foreign import javascript unsafe "$r = [$1]"
--    toArray1 :: JSVal -> IO (JSArray a)


--vecArray :: [Vector n x] -> IO (JSArray (Vector n x))
--vecArray vecs = mapM (Vec)
