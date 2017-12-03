{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module SmallGL.Types where

import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.TypeLits
import JavaScript.WebGL
import JavaScript.Object.Internal
import JavaScript.JSON.Types.Generic ()
import JavaScript.JSON.Types.Instances (FromJSON(..),ToJSON(..),fromJSON)
import JavaScript.JSON.Types.Internal (Result (..),SomeValue (..))
import GHC.Generics
--import GHC.TypeLits
import GHCJS.Types (JSVal, jsval, JSString)
import GHCJS.Marshal (FromJSVal(..), ToJSVal(..))
import GHCJS.Marshal.Pure (PFromJSVal(..), PToJSVal(..))
import Data.Coerce (coerce)
import Unsafe.Coerce (unsafeCoerce)
import SmallGL.Shader


-- | This variable is used to control how smooth is fading near the far clipping plane.
--   such that object fades when squared distance goes from fadeConst - 1 to fadeConst.
fadeConst :: Double
fadeConst = 10



data RenderingProgram = RenderingProgram
  { shader      :: !ShaderProgram
  , uProjLoc    :: !WebGLUniformLocation
  , uViewLoc    :: !WebGLUniformLocation
  , uCustomLoc3 :: WebGLUniformLocation
  , uCustomLoc4 :: WebGLUniformLocation
  , uCustomLoc5 :: WebGLUniformLocation
  }

uSunDirLoc :: RenderingProgram -> WebGLUniformLocation
uSunDirLoc = uCustomLoc3

uSamplerLoc :: RenderingProgram -> WebGLUniformLocation
uSamplerLoc = uCustomLoc3

uClippingDistLoc :: RenderingProgram -> WebGLUniformLocation
uClippingDistLoc = uCustomLoc4

uTexOpacityLoc :: RenderingProgram -> WebGLUniformLocation
uTexOpacityLoc = uCustomLoc5

-- | Id of an object within rendering system
--    (should be kept by other components, but not exposed outside viewer).
newtype RenderedObjectId = RenderedObjectId Int
    deriving (PToJSVal, ToJSVal, PFromJSVal, FromJSVal, Eq, Ord)

-- | Project camera space coordinates to screen normalized coordinates
newtype ProjMatrix = ProjM { getProjM :: Mat44f }
-- | Project global coordinates to camera space coordinates
newtype ViewMatrix = ViewM { getViewM :: Mat44f }

projMToClippingDist :: ProjMatrix -> Scf
projMToClippingDist (ProjM m) = f
  where
    a = 3:!3:!Z !. m
    b = 3:!4:!Z !. m
    f = b / (a + 1)
    -- n = b / (a - 1)

-- | Rendering modes supported by the engine
data RenderingMode
  = ModePoints
  | ModeLines
  | ModeColored
  | ModeTextured
  deriving (Generic, Show, Eq)

instance FromJSON   RenderingMode
instance ToJSON     RenderingMode
instance PFromJSVal RenderingMode where
    pFromJSVal v = case fromJSON (SomeValue v) of
        Error _ -> ModeColored
        Success r -> r
instance PToJSVal   RenderingMode where
    pToJSVal = coerce . toJSON
instance FromJSVal  RenderingMode where
    fromJSVal v = pure $ case fromJSON (SomeValue v) of
        Error _ -> Nothing
        Success r -> Just r
    fromJSValUnchecked = pure . pFromJSVal
instance ToJSVal    RenderingMode where
    toJSVal = pure . coerce . toJSON


type ModePoints   = 'ModePoints
type ModeLines    = 'ModeLines
type ModeColored  = 'ModeColored
type ModeTextured = 'ModeTextured


-- | Coordinate and normal array contains two 4D (homogeneous) components:
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

-- | Selection-rendering Ids of objects.
--   Default value is 0xFFFFFFFF meaning there is no object, or object is not selectable.
--   Selection Ids are provided by a call site
newtype SelectIds (n :: Nat) = SelectIds (IODataFrame GLuint '[n])

data ObjRenderingData (m :: RenderingMode) where
  ObjPointData    :: forall n . KnownDim n
                  => !(Coords n) -> !(Colors n) -> !GLuint
                  -> ObjRenderingData ModePoints
  ObjLineData     :: forall n k . (KnownDim n, KnownDim k)
                  => !(Coords n) -> !(Colors n) -> !GLuint -> !(Indices k)
                  -> ObjRenderingData ModeLines
  ObjColoredData  :: forall n k . (KnownDim n, KnownDim k)
                  => !(CoordsNormals n) -> !(Colors n) -> !GLuint -> !(Indices k)
                  -> ObjRenderingData ModeColored
  ObjTexturedData :: forall n k . (KnownDim n, KnownDim k)
                  => !(CoordsNormals n) -> !(TexCoords n) -> !GLuint -> !(Indices k)
                  -> ObjRenderingData ModeTextured

-- | Get number of vertices
ordVertexNum :: ObjRenderingData m -> Int
ordVertexNum (ObjPointData   _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _) = dimVal' @n
ordVertexNum (ObjLineData    _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _ _) = dimVal' @n
ordVertexNum (ObjColoredData _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _ _) = dimVal' @n
ordVertexNum (ObjTexturedData _ (TexCoords (_ :: IODataFrame GLushort '[2,n])) _ _) = dimVal' @n


-- | Get number of indices
ordIndexNum :: ObjRenderingData m -> Int
ordIndexNum (ObjPointData    _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _) = dimVal' @n
ordIndexNum (ObjLineData     _ _ _ (Indices (_ :: IODataFrame GLushort '[k]))) = dimVal' @k
ordIndexNum (ObjColoredData  _ _ _ (Indices (_ :: IODataFrame GLushort '[k]))) = dimVal' @k
ordIndexNum (ObjTexturedData _ _ _ (Indices (_ :: IODataFrame GLushort '[k]))) = dimVal' @k

-- | Get rendering mode at term level
orenderingMode :: ObjRenderingData m -> RenderingMode
orenderingMode ObjPointData {}    = ModePoints
orenderingMode ObjLineData {}     = ModeLines
orenderingMode ObjColoredData {}  = ModeColored
orenderingMode ObjTexturedData {} = ModeTextured

data RenderingData (m :: RenderingMode) where
  PointData    :: forall n . KnownDim n
               => !(Coords n) -> !(Colors n) -> !(SelectIds n)
               -> RenderingData ModePoints
  LineData     :: forall n k . (KnownDim n, KnownDim k)
               => !(Coords n) -> !(Colors n) -> !(SelectIds n) -> !(Indices k)
               -> RenderingData ModeLines
  ColoredData  :: forall n k . (KnownDim n, KnownDim k)
               => !(CoordsNormals n) -> !(Colors n) -> !(SelectIds n) -> !(Indices k)
               -> RenderingData ModeColored
  TexturedData :: forall n k . (KnownDim n, KnownDim k)
               => !(CoordsNormals n) -> !(TexCoords n) -> !(SelectIds n) -> !(Indices k)
               -> RenderingData ModeTextured

-- | Get number of vertices
rdVertexNum :: RenderingData m -> Int
rdVertexNum (PointData   _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _) = dimVal' @n
rdVertexNum (LineData    _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _ _) = dimVal' @n
rdVertexNum (ColoredData _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _ _) = dimVal' @n
rdVertexNum (TexturedData _ (TexCoords (_ :: IODataFrame GLushort '[2,n])) _ _) = dimVal' @n


-- | Get number of indices
rdIndexNum :: RenderingData m -> Int
rdIndexNum (PointData    _ (Colors (_ :: IODataFrame GLubyte '[4,n])) _) = dimVal' @n
rdIndexNum (LineData     _ _ _ (Indices (_ :: IODataFrame GLushort '[k]))) = dimVal' @k
rdIndexNum (ColoredData  _ _ _ (Indices (_ :: IODataFrame GLushort '[k]))) = dimVal' @k
rdIndexNum (TexturedData _ _ _ (Indices (_ :: IODataFrame GLushort '[k]))) = dimVal' @k

-- | Get rendering mode at term level
renderingMode :: RenderingData m -> RenderingMode
renderingMode PointData {}    = ModePoints
renderingMode LineData {}     = ModeLines
renderingMode ColoredData {}  = ModeColored
renderingMode TexturedData {} = ModeTextured


-- | All WebGL information to render a geometry
data WebGLRenderingData (m :: RenderingMode) where
  WebGLPointData    :: !GLsizei -> WebGLBuffer -> WebGLBuffer -> WebGLBuffer
                    -> WebGLRenderingData ModePoints
  WebGLLineData     :: !GLsizei -> WebGLBuffer -> WebGLBuffer -> WebGLBuffer -> WebGLBuffer
                    -> WebGLRenderingData ModeLines
  WebGLColoredData  :: !GLsizei -> WebGLBuffer -> WebGLBuffer -> WebGLBuffer -> WebGLBuffer
                    -> WebGLRenderingData ModeColored
  WebGLTexturedData :: !GLsizei -> WebGLBuffer -> WebGLBuffer -> WebGLBuffer -> WebGLBuffer
                    -> WebGLRenderingData ModeTextured

wglSeqLen :: WebGLRenderingData m -> GLsizei
wglSeqLen (WebGLPointData    n _ _ _  ) = n
wglSeqLen (WebGLLineData     n _ _ _ _) = n
wglSeqLen (WebGLColoredData  n _ _ _ _) = n
wglSeqLen (WebGLTexturedData n _ _ _ _) = n

type family IsSolid (m :: RenderingMode) :: Bool where
    IsSolid 'ModePoints   = 'False
    IsSolid 'ModeLines    = 'False
    IsSolid 'ModeColored  = 'True
    IsSolid 'ModeTextured = 'True

type family HasTexture (m :: RenderingMode) :: Bool where
    HasTexture 'ModeTextured = 'True
    HasTexture _             = 'False

type family IsIndexedDraw (m :: RenderingMode) :: Bool where
    IsIndexedDraw 'ModePoints   = 'False
    IsIndexedDraw 'ModeLines    = 'True
    IsIndexedDraw 'ModeColored  = 'True
    IsIndexedDraw 'ModeTextured = 'True


wglCoordsBuf :: IsSolid m ~ 'False
             => WebGLRenderingData m -> WebGLBuffer
wglCoordsBuf (WebGLPointData _ b _ _) = b
wglCoordsBuf (WebGLLineData  _ b _ _ _) = b

wglCoordsNormalsBuf :: IsSolid m ~ 'True
                    => WebGLRenderingData m -> WebGLBuffer
wglCoordsNormalsBuf (WebGLColoredData  _ b _ _ _) = b
wglCoordsNormalsBuf (WebGLTexturedData _ b _ _ _) = b

wglColorsBuf :: HasTexture m ~ 'False
             => WebGLRenderingData m -> WebGLBuffer
wglColorsBuf (WebGLPointData    _ _ b _) = b
wglColorsBuf (WebGLLineData     _ _ b _ _) = b
wglColorsBuf (WebGLColoredData  _ _ b _ _) = b

wglTexCoordsBuf :: HasTexture m ~ 'True
                => WebGLRenderingData m -> WebGLBuffer
wglTexCoordsBuf (WebGLTexturedData _ _ b _ _) = b


wglIndicesBuf :: IsIndexedDraw m ~ 'True
                => WebGLRenderingData m -> WebGLBuffer
wglIndicesBuf (WebGLLineData     _ _ _ _ b) = b
wglIndicesBuf (WebGLColoredData  _ _ _ _ b) = b
wglIndicesBuf (WebGLTexturedData _ _ _ _ b) = b


wglSelectorIdsBuf :: WebGLRenderingData m -> WebGLBuffer
wglSelectorIdsBuf (WebGLPointData    _ _ b _  ) = b
wglSelectorIdsBuf (WebGLLineData     _ _ _ b _) = b
wglSelectorIdsBuf (WebGLColoredData  _ _ _ b _) = b
wglSelectorIdsBuf (WebGLTexturedData _ _ _ b _) = b


-- * Shader attribute locations
--
--   Trying to keep them as constant as possible

-- | constant attribute locations for all shaders (set if necessary)
--
attrLocCoords, attrLocNormals, attrLocColors, attrLocTexCoords, attrLocSelIds :: GLuint
attrLocCoords    = 0
attrLocNormals   = 1
attrLocColors    = 2
attrLocTexCoords = 3
attrLocSelIds    = 1

setCoordsNormalsBuf :: WebGLRenderingContext -> IO ()
setCoordsNormalsBuf gl = do
    vertexAttribPointer gl attrLocCoords  4 gl_FLOAT False 32 0
    vertexAttribPointer gl attrLocNormals 4 gl_FLOAT False 32 16

setCoordsNoNormalsBuf :: WebGLRenderingContext -> IO ()
setCoordsNoNormalsBuf gl =
    vertexAttribPointer gl attrLocCoords  4 gl_FLOAT False 32 0

setCoordsBuf :: WebGLRenderingContext -> IO ()
setCoordsBuf gl = vertexAttribPointer gl attrLocCoords  4 gl_FLOAT False 16 0

setColorsBuf :: WebGLRenderingContext -> IO ()
setColorsBuf gl = vertexAttribPointer gl attrLocColors 4 gl_UNSIGNED_BYTE True 4 0

setTexCoordsBuf :: WebGLRenderingContext -> IO ()
setTexCoordsBuf gl = vertexAttribPointer gl attrLocTexCoords 2 gl_UNSIGNED_SHORT True 4 0

setSelIdsBuf :: WebGLRenderingContext -> IO ()
setSelIdsBuf gl = vertexAttribPointer gl attrLocSelIds 4 gl_UNSIGNED_BYTE True 4 0

enableCoordsNormalsBuf :: WebGLRenderingContext -> IO ()
enableCoordsNormalsBuf gl = do
  enableVertexAttribArray gl attrLocCoords
  enableVertexAttribArray gl attrLocNormals

disableCoordsNormalsBuf :: WebGLRenderingContext -> IO ()
disableCoordsNormalsBuf gl = do
  disableVertexAttribArray gl attrLocCoords
  disableVertexAttribArray gl attrLocNormals

enableCoordsBuf :: WebGLRenderingContext -> IO ()
enableCoordsBuf gl = enableVertexAttribArray gl attrLocCoords

disableCoordsBuf :: WebGLRenderingContext -> IO ()
disableCoordsBuf gl = disableVertexAttribArray gl attrLocCoords

enableColorsBuf :: WebGLRenderingContext -> IO ()
enableColorsBuf gl = enableVertexAttribArray gl attrLocColors

disableColorsBuf :: WebGLRenderingContext -> IO ()
disableColorsBuf gl = disableVertexAttribArray gl attrLocColors

enableTexCoordsBuf :: WebGLRenderingContext -> IO ()
enableTexCoordsBuf gl = enableVertexAttribArray gl attrLocTexCoords

disableTexCoordsBuf :: WebGLRenderingContext -> IO ()
disableTexCoordsBuf gl = disableVertexAttribArray gl attrLocTexCoords

enableSelIdsBuf :: WebGLRenderingContext -> IO ()
enableSelIdsBuf gl = enableVertexAttribArray gl attrLocSelIds

disableSelIdsBuf :: WebGLRenderingContext -> IO ()
disableSelIdsBuf gl = disableVertexAttribArray gl attrLocSelIds


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


instance ToJSVal (ObjRenderingData m) where
    toJSVal d = do
      o <- create
      unsafeSetProp "n"    (pToJSVal $ ordVertexNum d) o
      unsafeSetProp "mode" (pToJSVal $ orenderingMode d) o
      () <- case d of
        (ObjPointData coords colors selectId) -> do
          unsafeSetProp "coords"   (coerce coords) o
          unsafeSetProp "colors"   (coerce colors) o
          unsafeSetProp "selectId" (pToJSVal selectId) o
        (ObjLineData coords colors selectId indices) -> do
          unsafeSetProp "k"        (pToJSVal $ ordIndexNum d) o
          unsafeSetProp "coords"   (coerce coords) o
          unsafeSetProp "colors"   (coerce colors) o
          unsafeSetProp "selectId" (pToJSVal selectId) o
          unsafeSetProp "indices"  (coerce indices) o
        (ObjColoredData coordsnormals colors selectId indices) -> do
          unsafeSetProp "k"              (pToJSVal $ ordIndexNum d) o
          unsafeSetProp "coordsnormals"  (coerce coordsnormals) o
          unsafeSetProp "colors"         (coerce colors) o
          unsafeSetProp "selectId"       (pToJSVal selectId) o
          unsafeSetProp "indices"        (coerce indices) o
        (ObjTexturedData coordsnormals texcoords selectId indices) -> do
          unsafeSetProp "k"              (pToJSVal $ ordIndexNum d) o
          unsafeSetProp "coordsnormals"  (coerce coordsnormals) o
          unsafeSetProp "texcoords"      (coerce texcoords) o
          unsafeSetProp "selectId"       (pToJSVal selectId) o
          unsafeSetProp "indices"        (coerce indices) o
      return $ jsval o

instance FromJSVal (ObjRenderingData m) where
    fromJSVal jsv = do
      mn    <- unsafeGetProp "n" o >>= fromJSVal
      mmode <- unsafeGetProp "mode" o >>= fromJSVal
      mselectId <- unsafeGetProp "selectId" o >>= fromJSVal :: IO (Maybe GLuint)
      case (,) <$> (mn >>= someIntNatVal) <*> mmode of

        Nothing -> return Nothing

        Just (SomeIntNat (_::Proxy n), ModePoints) ->
          case unsafeCoerce (Evidence :: Evidence (m ~ m)) of
            (Evidence :: Evidence (m ~ ModePoints)) -> do
              mcoords <- getDF "coords" o :: IO (Maybe (IODataFrame Float '[4,n]))
              mcolors <- getDF "colors" o
              return $ ObjPointData <$> (Coords <$> mcoords) <*> (Colors <$> mcolors) <*> mselectId

        Just (SomeIntNat (_::Proxy n), ModeLines) -> do
          mk    <- unsafeGetProp "k" o >>= fromJSVal
          case (unsafeCoerce (Evidence :: Evidence (m ~ m)), mk >>= someIntNatVal) of
            (_, Nothing) -> return Nothing
            (  Evidence :: Evidence (m ~ ModeLines)
             , Just (SomeIntNat (_::Proxy k))) -> do
              mcoords  <- getDF "coords" o :: IO (Maybe (IODataFrame Float '[4,n]))
              mcolors  <- getDF "colors" o
              mindices <- getDF "indices" o :: IO (Maybe (IODataFrame GLushort '[k]))
              return $ ObjLineData <$> (Coords  <$> mcoords)
                                <*> (Colors  <$> mcolors)
                                <*> mselectId
                                <*> (Indices <$> mindices)

        Just (SomeIntNat (_::Proxy n), ModeColored) -> do
          mk    <- unsafeGetProp "k" o >>= fromJSVal
          case (unsafeCoerce (Evidence :: Evidence (m ~ m)), mk >>= someIntNatVal) of
            (_, Nothing) -> return Nothing
            (  Evidence :: Evidence (m ~ ModeColored)
             , Just (SomeIntNat (_::Proxy k))) -> do
              mcoords  <- getDF "coordsnormals" o :: IO (Maybe (IODataFrame Float '[4,2,n]))
              mcolors  <- getDF "colors" o
              mindices <- getDF "indices" o :: IO (Maybe (IODataFrame GLushort '[k]))
              return $ ObjColoredData <$> (CoordsNormals <$> mcoords)
                                   <*> (Colors    <$> mcolors)
                                   <*> mselectId
                                   <*> (Indices   <$> mindices)

        Just (SomeIntNat (_::Proxy n), ModeTextured) -> do
          mk    <- unsafeGetProp "k" o >>= fromJSVal
          case (unsafeCoerce (Evidence :: Evidence (m ~ m)), mk >>= someIntNatVal) of
            (_, Nothing) -> return Nothing
            (  Evidence :: Evidence (m ~ ModeTextured)
             , Just (SomeIntNat (_::Proxy k))) -> do
              mcoords  <- getDF "coordsnormals" o :: IO (Maybe (IODataFrame Float '[4,2,n]))
              mtexcoords  <- getDF "texcoords" o
              mindices <- getDF "indices" o :: IO (Maybe (IODataFrame GLushort '[k]))
              return $ ObjTexturedData <$> (CoordsNormals <$> mcoords)
                                    <*> (TexCoords  <$> mtexcoords)
                                    <*> mselectId
                                    <*> (Indices <$> mindices)

      where
        o = Object jsv
        getDF :: forall t ds . JSString -> Object -> IO (Maybe (IODataFrame t ds))
        getDF n obj = do
          mjdf <- unsafeGetProp n obj >>= fromJSVal :: IO (Maybe JSVal)
          return $ coerce <$> mjdf





--initTexture :: WebGLRenderingContext -> Either TexImageSource (TypedArray GLubyte, (GLsizei, GLsizei)) -> IO WebGLTexture
--initTexture gl texdata = do
--    tex <- createTexture gl
--    bindTexture gl gl_TEXTURE_2D tex
--    case texdata of
--        Left img -> do
--            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
--            texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
--        Right (arr, (w,h)) -> do
--            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 0
--            texImage2D gl gl_TEXTURE_2D 0 gl_RGBA w h 0 gl_RGBA gl_UNSIGNED_BYTE (Just arr)
--    setTexParameters gl
--    bindTexture gl gl_TEXTURE_2D nullRef
--    return tex

setTexParameters :: WebGLRenderingContext -> IO ()
setTexParameters gl = do
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_S $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_WRAP_T $ fromIntegral gl_CLAMP_TO_EDGE
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MAG_FILTER $ fromIntegral gl_NEAREST
    texParameteri gl gl_TEXTURE_2D gl_TEXTURE_MIN_FILTER $ fromIntegral gl_NEAREST

--updateTexture :: WebGLRenderingContext
--              -> Either TexImageSource (TypedArray GLubyte, (GLsizei, GLsizei))
--              -> WebGLTexture
--              -> IO ()
--updateTexture gl texdata tex = do
--    bindTexture gl gl_TEXTURE_2D tex
--    case texdata of
--        Left img -> do
--            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
--            texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE img
--        Right (arr, (w,h)) -> do
--            pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 0
--            texImage2D gl gl_TEXTURE_2D 0 gl_RGBA w h 0 gl_RGBA gl_UNSIGNED_BYTE (Just arr)
--    bindTexture gl gl_TEXTURE_2D nullRef

