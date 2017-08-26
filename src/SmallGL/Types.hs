{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SmallGL.Types where


import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.TypeLits
import JavaScript.WebGL
import JavaScript.Object.Internal
import GHCJS.Types (JSVal, jsval)
import GHCJS.Marshal (FromJSVal(..), ToJSVal(..))
import GHCJS.Marshal.Pure (PFromJSVal(..), PToJSVal(..))
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)


-- | Id of an object within rendering system
--    (should be kept by other components, but not exposed outside viewer).
newtype RenderedObjectId = RenderedObjectId Int
    deriving (PToJSVal, ToJSVal, PFromJSVal, FromJSVal, Eq, Ord)

-- | Project camera space coordinates to screen normalized coordinates
newtype ProjMatrix = ProjM { getProjM :: Mat44f }
-- | Project global coordinates to camera space coordinates
newtype ViewMatrix = ViewM { getViewM :: Mat44f }




-- | Coordinate and normal array contains two 4D (homogenious) components:
--
--    * Points (4th coordinate is 1)
--    * Normals (4th coordinate is 0, norm == 1)
newtype CoordsNormals (n :: Nat) = CoordsNormals (IODataFrame Float '[4, 2, n])

-- | Coordinate array
newtype Coords (n :: Nat) = Coords (IODataFrame Float '[4, n])

-- | Specifying colors as 4D unsigned bytes
newtype Colors (n :: Nat) = Colors (IODataFrame GLubyte '[4, n])

-- | Mapping textures
newtype TexCoords (n :: Nat) = TexCoords (IODataFrame GLushort '[2, n])

-- | Draw elements by these indices (note, maxBound == 65535 for GLushort)
newtype Indices (m :: Nat) = Indices (IODataFrame GLushort '[m])


data ColoredPointData = forall n . KnownDim n
  => ColoredPointData (Coords n) (Colors n)

data ColoredLineData = forall n m . (KnownDim n, KnownDim m)
  => ColoredLineData (Coords n) (Colors n) (Indices m)

-- | All data for a solid-colored object in one existential place
data ColoredData = forall n m . (KnownDim n, KnownDim m)
  => ColoredData (CoordsNormals n) (Colors n) (Indices m)

-- | All data for an object with texture in one existential place
data TexturedData = forall n m . (KnownDim n, KnownDim m)
  => TexturedData (CoordsNormals n) (TexCoords n) (Indices m)

-- | Get number of vertices
cdVertexNum :: ColoredData -> Int
cdVertexNum (ColoredData _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _) = dimVal' @n

-- | Get number of indices
cdIndexNum :: ColoredData -> Int
cdIndexNum (ColoredData _ _ (Indices (_ :: IODataFrame GLushort '[m]))) = dimVal' @m


-- | All WebGL information to render a geometry with colors (but no textures)
data ColoredGeometryWebGLData
  = ColoredGeometryWebGLData
  { cgIdxLen           :: GLsizei
  , cgCoordsNormalsBuf :: WebGLBuffer
  , cgColorsBuf        :: WebGLBuffer
  , cgIndicesBuf       :: WebGLBuffer
  }




-- * Shader attribute locations
--
--   Trying to keep them as constant as possible

-- | constant attribute locations for all shaders (set if necessary)
--
attrLocCoords, attrLocNormals, attrLocColors, attrLocTexCoords :: GLuint
attrLocCoords    = 0
attrLocNormals   = 1
attrLocColors    = 2
attrLocTexCoords = 3

setCoordsNormalsBuf :: WebGLRenderingContext -> IO ()
setCoordsNormalsBuf gl = do
    vertexAttribPointer gl attrLocCoords  4 gl_FLOAT False 32 0
    vertexAttribPointer gl attrLocNormals 4 gl_FLOAT False 32 16

setColorsBuf :: WebGLRenderingContext -> IO ()
setColorsBuf gl = vertexAttribPointer gl attrLocColors 4 gl_UNSIGNED_BYTE True 4 0

setTexCoordsBuf :: WebGLRenderingContext -> IO ()
setTexCoordsBuf gl = vertexAttribPointer gl attrLocTexCoords 2 gl_UNSIGNED_SHORT True 4 0

enableCoordsNormalsBuf :: WebGLRenderingContext -> IO ()
enableCoordsNormalsBuf gl = do
  enableVertexAttribArray gl attrLocCoords
  enableVertexAttribArray gl attrLocNormals

disableCoordsNormalsBuf :: WebGLRenderingContext -> IO ()
disableCoordsNormalsBuf gl = do
  disableVertexAttribArray gl attrLocCoords
  disableVertexAttribArray gl attrLocNormals

enableColorsBuf :: WebGLRenderingContext -> IO ()
enableColorsBuf gl = enableVertexAttribArray gl attrLocColors

disableColorsBuf :: WebGLRenderingContext -> IO ()
disableColorsBuf gl = disableVertexAttribArray gl attrLocColors

enableTexCoordsBuf :: WebGLRenderingContext -> IO ()
enableTexCoordsBuf gl = enableVertexAttribArray gl attrLocTexCoords

disableTexCoordsBuf :: WebGLRenderingContext -> IO ()
disableTexCoordsBuf gl = disableVertexAttribArray gl attrLocTexCoords

-- | void bufferData(GLenum target, BufferDataSource? data, GLenum usage) (OpenGL ES 2.0 §2.9, man page)
--   Set the size of the currently bound WebGLBuffer object for the passed target to the size of the passed data,
--   then write the contents of data to the buffer object.
foreign import javascript unsafe "$1.bufferData($2, $3, $4)"
    bufferData' :: WebGLRenderingContext -> GLenum -> IODataFrame t ds -> GLenum -> IO ()

-- | void bufferSubData(GLenum target, GLintptr offset, BufferDataSource? data) (OpenGL ES 2.0 §2.9, man page)
--   For the WebGLBuffer object bound to the passed target write the passed data starting at the passed offset.
--   If the data would be written past the end of the buffer object an INVALID_VALUE error is generated.
--   If data is null then an INVALID_VALUE error is generated.
foreign import javascript unsafe "$1.bufferSubData($2, $3, $4)"
    bufferSubData' :: WebGLRenderingContext -> GLenum -> Int -> IODataFrame t ds -> IO ()


foreign import javascript unsafe "$1.subarray($2,$3)"
    js_unsafeSubArrayFreeze :: IODataFrame t asbs -> Int -> Int -> IO JSVal

unsafeSubArrayFreeze :: forall t (as :: [Nat]) (b' :: Nat) (b :: Nat) (bs :: [Nat]) (asbs :: [Nat])
                   . ( ConcatList as (b :+ bs) asbs
                     , Dimensions (b :+ bs)
                     , Dimensions (as +: b')
                     , Dimensions as
                     )
                   => IODataFrame t asbs -> Idx (b :+ bs) -> IO (DataFrame t (as +: b'))
unsafeSubArrayFreeze df i
  | off <- fromEnum i * dimVal (dim @as)
  = unsafeCoerce <$> js_unsafeSubArrayFreeze df off (off + totalDim (Proxy @(as +: b')))


unsafeSubArray :: forall t (as :: [Nat]) (b' :: Nat) (b :: Nat) (bs :: [Nat]) (asbs :: [Nat])
                . ( ConcatList as (b :+ bs) asbs
                  , Dimensions (b :+ bs)
                  , Dimensions (as +: b')
                  , Dimensions as
                  )
               => IODataFrame t asbs -> Idx (b :+ bs) -> IO (IODataFrame t (as +: b'))
unsafeSubArray df i
  | off <- fromEnum i * dimVal (dim @as)
  = unsafeCoerce <$> js_unsafeSubArrayFreeze df off (off + totalDim (Proxy @(as +: b')))



unsafeArrayThaw :: DataFrame t (a :+ as) -> IO (IODataFrame t (a :+ as))
unsafeArrayThaw df = pure (unsafeCoerce df)



instance ToJSVal ColoredData where
    toJSVal (ColoredData (CoordsNormals (cns :: IODataFrame Float '[4,2,n] ))
                         (Colors colors)
                         (Indices (is :: IODataFrame GLushort '[m]))
            ) = do
        o <- create
        unsafeSetProp "n"  (pToJSVal (dimVal' @n)) o
        unsafeSetProp "m"  (pToJSVal (dimVal' @m)) o
        unsafeSetProp "coordsnormals" (coerce cns) o
        unsafeSetProp "colors"        (coerce colors) o
        unsafeSetProp "indices"       (coerce is) o
        return $ jsval o

instance FromJSVal ColoredData where
    fromJSValUnchecked jsv = do
        n <- unsafeGetProp "n" o >>= fromJSValUnchecked
        m <- unsafeGetProp "m" o >>= fromJSValUnchecked
        case (,) <$> someIntNatVal n <*> someIntNatVal m of
          Nothing -> error "Could not get DataFrame dimensions"
          Just (SomeIntNat (_::Proxy n), SomeIntNat (_::Proxy m)) -> do
            crdnrms <- coerce <$> unsafeGetProp "coordsnormals" o
            colors  <- coerce <$> unsafeGetProp "colors" o
            indices <- coerce <$> unsafeGetProp "indices" o
            return $ ColoredData
              (CoordsNormals (crdnrms :: IODataFrame Float '[4,2,n] ))
              (Colors colors)
              (Indices (indices :: IODataFrame GLushort '[m]))
      where
        o = Object jsv
    fromJSVal jsv = do
        n <- unsafeGetProp "n" o >>= fromJSVal
        m <- unsafeGetProp "m" o >>= fromJSVal
        case (,) <$> (n >>= someIntNatVal) <*> (m >>= someIntNatVal) of
          Nothing -> return Nothing
          Just (SomeIntNat (_::Proxy n), SomeIntNat (_::Proxy m)) -> do
            crdnrms <- unsafeGetProp "coordsnormals" o >>= fromJSVal :: IO (Maybe JSVal)
            colors  <- unsafeGetProp "colors"        o >>= fromJSVal :: IO (Maybe JSVal)
            indices <- unsafeGetProp "indices"       o >>= fromJSVal :: IO (Maybe JSVal)
            return $ case (,,) <$> fmap coerce crdnrms
                               <*> fmap coerce colors
                               <*> fmap coerce indices of
              Nothing -> Nothing
              Just (a,b,c) -> Just $ ColoredData
                 (CoordsNormals (a :: IODataFrame Float '[4,2,n] ))
                 (Colors b)
                 (Indices (c :: IODataFrame GLushort '[m]))
      where
        o = Object jsv


instance ToJSVal ColoredLineData where
    toJSVal (ColoredLineData (Coords (cns :: IODataFrame Float '[4,n] ))
                             (Colors colors)
                             (Indices (is :: IODataFrame GLushort '[m]))
            ) = do
        o <- create
        unsafeSetProp "n"  (pToJSVal (dimVal' @n)) o
        unsafeSetProp "m"  (pToJSVal (dimVal' @m)) o
        unsafeSetProp "coords" (coerce cns) o
        unsafeSetProp "colors"        (coerce colors) o
        unsafeSetProp "indices"       (coerce is) o
        return $ jsval o

instance FromJSVal ColoredLineData where
    fromJSValUnchecked jsv = do
        n <- unsafeGetProp "n" o >>= fromJSValUnchecked
        m <- unsafeGetProp "m" o >>= fromJSValUnchecked
        case (,) <$> someIntNatVal n <*> someIntNatVal m of
          Nothing -> error "Could not get DataFrame dimensions"
          Just (SomeIntNat (_::Proxy n), SomeIntNat (_::Proxy m)) -> do
            crdnrms <- coerce <$> unsafeGetProp "coords" o
            colors  <- coerce <$> unsafeGetProp "colors" o
            indices <- coerce <$> unsafeGetProp "indices" o
            return $ ColoredLineData
              (Coords (crdnrms :: IODataFrame Float '[4,n] ))
              (Colors colors)
              (Indices (indices :: IODataFrame GLushort '[m]))
      where
        o = Object jsv
    fromJSVal jsv = do
        n <- unsafeGetProp "n" o >>= fromJSVal
        m <- unsafeGetProp "m" o >>= fromJSVal
        case (,) <$> (n >>= someIntNatVal) <*> (m >>= someIntNatVal) of
          Nothing -> return Nothing
          Just (SomeIntNat (_::Proxy n), SomeIntNat (_::Proxy m)) -> do
            crdnrms <- unsafeGetProp "coords"  o >>= fromJSVal :: IO (Maybe JSVal)
            colors  <- unsafeGetProp "colors"  o >>= fromJSVal :: IO (Maybe JSVal)
            indices <- unsafeGetProp "indices" o >>= fromJSVal :: IO (Maybe JSVal)
            return $ case (,,) <$> fmap coerce crdnrms
                               <*> fmap coerce colors
                               <*> fmap coerce indices of
              Nothing -> Nothing
              Just (a,b,c) -> Just $ ColoredLineData
                 (Coords (a :: IODataFrame Float '[4,n] ))
                 (Colors b)
                 (Indices (c :: IODataFrame GLushort '[m]))
      where
        o = Object jsv


instance ToJSVal ColoredPointData where
    toJSVal (ColoredPointData (Coords (cns :: IODataFrame Float '[4,n] ))
                              (Colors colors)
            ) = do
        o <- create
        unsafeSetProp "n"  (pToJSVal (dimVal' @n)) o
        unsafeSetProp "coords" (coerce cns) o
        unsafeSetProp "colors"        (coerce colors) o
        return $ jsval o

instance FromJSVal ColoredPointData where
    fromJSValUnchecked jsv = do
        n <- unsafeGetProp "n" o >>= fromJSValUnchecked
        case someIntNatVal n of
          Nothing -> error "Could not get DataFrame dimensions"
          Just (SomeIntNat (_::Proxy n)) -> do
            crdnrms <- coerce <$> unsafeGetProp "coords" o
            colors  <- coerce <$> unsafeGetProp "colors" o
            return $ ColoredPointData
              (Coords (crdnrms :: IODataFrame Float '[4,n] ))
              (Colors colors)
      where
        o = Object jsv
    fromJSVal jsv = do
        n <- unsafeGetProp "n" o >>= fromJSVal
        case n >>= someIntNatVal of
          Nothing -> return Nothing
          Just (SomeIntNat (_::Proxy n)) -> do
            crdnrms <- unsafeGetProp "coords" o >>= fromJSVal :: IO (Maybe JSVal)
            colors  <- unsafeGetProp "colors" o >>= fromJSVal :: IO (Maybe JSVal)
            return $ case (,) <$> fmap coerce crdnrms
                              <*> fmap coerce colors of
              Nothing -> Nothing
              Just (a,b) -> Just $ ColoredPointData
                 (Coords (a :: IODataFrame Float '[4,n] ))
                 (Colors b)
      where
        o = Object jsv


