{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module SmallGL.Types where


import Reflex.Dom.Widget.Animation as Animation
import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import JavaScript.WebGL


-- import Commons


data SmallGLInput a where
    ViewPortResize      :: SmallGLInput Animation.ResizeEvent
    -- ^ Every time windows is resized
    ProjTransformChange :: SmallGLInput ProjMatrix
    -- ^ Camera updates of viewport projection
    ViewTransformChange :: SmallGLInput ViewMatrix
    -- ^ Camera motions


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


cdVertexNum :: ColoredData -> Int
cdVertexNum (ColoredData _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _) = dimVal' @n

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
attrLocCoords    = 1
attrLocNormals   = 2
attrLocColors    = 3
attrLocTexCoords = 4

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


