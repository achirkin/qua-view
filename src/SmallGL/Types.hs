{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE JavaScriptFFI #-}
module SmallGL.Types where


import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.TypeLits
import JavaScript.WebGL
import GHCJS.Types (JSVal)
import Unsafe.Coerce (unsafeCoerce)


-- | Id of an object within rendering system
--    (should be kept by other components, but not exposed outside viewer).
newtype RenderedObjectId = RenderedObjectId Int
    deriving (Eq, Ord)

-- | Project camera space coordinates to screen normalized coordinates
newtype ProjMatrix = ProjM { getProjM :: Mat44f }
-- | Project global coordinates to camera space coordinates
newtype ViewMatrix = ViewM { getViewM :: Mat44f }




-- | Coordinate and normal array contains two 4D (homogenious) components:
--
--    * Points (4th coordinate is 1)
--    * Normals (4th coordinate is 0, norm == 1)
newtype CoordsNormals (n :: Nat) = CoordsNormals (IODataFrame Float '[4, 2, n])

-- | Specifying colors as 4D unsigned bytes
newtype Colors (n :: Nat) = Colors (IODataFrame GLubyte '[4, n])

-- | Mapping textures
newtype TexCoords (n :: Nat) = TexCoords (IODataFrame GLushort '[2, n])

-- | Draw elements by these indices (note, maxBound == 65535 for GLushort)
newtype Indices (m :: Nat) = Indices (IODataFrame GLushort '[m])

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

