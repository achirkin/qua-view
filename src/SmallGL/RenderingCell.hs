{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
module SmallGL.RenderingCell where



import JavaScript.WebGL


import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.Dimensions.Traverse.IO
import Numeric.TypeLits

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap


--import Commons
import SmallGL.Types
--import SmallGL.Shader




-- | Coollection of objects in a small area packed in a single set of WebGL buffers for drawing.
data RenderingCell = RenderingCell
  { rcData    :: !ColoredData
    -- ^ All the data on host that might be needed to setup webGL buffers
  , rcGPUData :: !ColoredGeometryWebGLData
    -- ^ All the WebGL buffers information
  , rcObjects :: !(IntMap RenderedObjRef)
    -- ^ Collection of the rendered objects, so that we can update, add, or delete objects dynamically.
  , rcContentDataLength  :: !Int
    -- ^ How many vertices are stored in the RenderingCell already
  , rcNextObjId :: !Int
  }

-- | How many indices are stored in the RenderingCell already
rcContentIndexLength :: RenderingCell -> Int
rcContentIndexLength = fromIntegral . cgIdxLen . rcGPUData

-- TODO: think about index and buffer limits on gpus!



-- | A reference information about an abstract rendered object
data RenderedObjRef = RenderedObjRef
  { roDataIdx    :: !Int
    -- ^ Index of an object-related data in the DataFrame of points and colors (outermost index).
    --   Note, indices in DataFrames start with 1.
  , roDataLength :: !Int
    -- ^ number of vertices in the object.
  , roIndexIdx   :: !Int
    -- ^ Starting index of object-related data in vertex indices data frame.
    --   Note, indices in DataFrames start with 1.
  , roIndexLength :: !Int
    -- ^ Number of indices in the index dataframe.
  }

type InitialVertexSize = 64
type InitialIndexSize = 128

-- | Build default empty rendering cell with some arrays pre-allocated
initRenderingCell :: WebGLRenderingContext -> IO RenderingCell
initRenderingCell = initRenderingCell' @InitialVertexSize @InitialIndexSize

initRenderingCell' :: forall (n :: Nat) (m :: Nat)
                    . (KnownDim n, KnownDim m)
                   => WebGLRenderingContext -> IO RenderingCell
initRenderingCell' gl = do
    crsnrs <- newDataFrame @_ @'[4,2,n]
    colors <- newDataFrame @_ @'[4,n]
    ixs    <- newDataFrame @_ @'[m]

    let cgIdxLen = 0
    -- create device buffers
    cgCoordsNormalsBuf <- createBuffer gl
    cgColorsBuf        <- createBuffer gl
    cgIndicesBuf       <- createBuffer gl

    -- send data to buffers
    bindBuffer gl gl_ARRAY_BUFFER cgCoordsNormalsBuf
    bufferData' gl gl_ARRAY_BUFFER crsnrs gl_STATIC_DRAW

    bindBuffer gl gl_ARRAY_BUFFER cgColorsBuf
    bufferData' gl gl_ARRAY_BUFFER colors gl_STATIC_DRAW

    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER cgIndicesBuf
    bufferData' gl gl_ELEMENT_ARRAY_BUFFER ixs gl_STATIC_DRAW

    let rcGPUData = ColoredGeometryWebGLData {..}
        rcData  = ColoredData (CoordsNormals crsnrs) (Colors colors) (Indices ixs)
        rcObjects = IntMap.empty
        rcContentDataLength = 0
        rcNextObjId = 0

    return RenderingCell {..}


-- | Make data arrays twice larger than they are atm.
growRenderingCellData :: WebGLRenderingContext -> RenderingCell -> IO RenderingCell
growRenderingCellData gl rc@RenderingCell {..}
    | ColoredData (CoordsNormals crsnrs)
                  (Colors (colors :: IODataFrame GLubyte '[4,n]))
                  ixs <- rcData
    , Evidence <- inferTimesKnownDim @2 @n
    = do
    -- cleanup existing webgl buffers
    deleteBuffer gl (cgCoordsNormalsBuf rcGPUData)
    deleteBuffer gl (cgColorsBuf rcGPUData)
    -- create new data buffers
    crsnrs' <- newDataFrame @_ @'[4,2,2*n]
    colors' <- newDataFrame @_ @'[4,2*n]

    -- copy old data
    copyMutableDataFrame crsnrs (1:!Z) crsnrs'
    copyMutableDataFrame colors (1:!Z) colors'

    -- create new WebGL buffers
    cgCoordsNormalsBuf' <- createBuffer gl
    cgColorsBuf'        <- createBuffer gl

    -- send data to buffers
    bindBuffer gl gl_ARRAY_BUFFER cgCoordsNormalsBuf'
    bufferData' gl gl_ARRAY_BUFFER crsnrs' gl_STATIC_DRAW

    bindBuffer gl gl_ARRAY_BUFFER cgColorsBuf'
    bufferData' gl gl_ARRAY_BUFFER colors' gl_STATIC_DRAW

    return rc
      { rcData = ColoredData (CoordsNormals crsnrs') (Colors colors') ixs
      , rcGPUData = rcGPUData
          { cgCoordsNormalsBuf = cgCoordsNormalsBuf'
          , cgColorsBuf = cgColorsBuf'
          }
      }

-- | Make index array twice larger than it is atm.
growRenderingCellIndices :: WebGLRenderingContext -> RenderingCell -> IO RenderingCell
growRenderingCellIndices gl rc@RenderingCell {..}
    | ColoredData crsnrs
                  colors
                  (Indices (ixs :: IODataFrame GLushort '[m])) <- rcData
    , Evidence <- inferTimesKnownDim @2 @m
    = do
    -- cleanup existing webgl buffers
    deleteBuffer gl (cgIndicesBuf rcGPUData)
    -- create new data buffers
    ixs' <- newDataFrame @_ @'[2*m]

    -- copy old data
    copyMutableDataFrame ixs (1:!Z) ixs'

    -- create new WebGL buffers
    cgIndicesBuf' <- createBuffer gl

    -- send data to buffers
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER cgIndicesBuf'
    bufferData' gl gl_ELEMENT_ARRAY_BUFFER ixs' gl_STATIC_DRAW

    return rc
      { rcData = ColoredData crsnrs colors (Indices ixs')
      , rcGPUData = rcGPUData
          { cgIndicesBuf = cgIndicesBuf'
          }
      }


-- | Add one more object to the cell
addRenderedObject :: WebGLRenderingContext
                  -> ColoredData
                  -> RenderingCell
                  -> IO (RenderedObjectId, RenderingCell)
addRenderedObject gl obj rc@RenderingCell{rcGPUData = gpuData}
    | cdVertexNum obj + rcContentDataLength rc > cdVertexNum (rcData rc)
      = growRenderingCellData gl rc >>= addRenderedObject gl obj
    | cdIndexNum obj + rcContentIndexLength rc > cdIndexNum (rcData rc)
      = growRenderingCellIndices gl rc >>= addRenderedObject gl obj
    | ColoredData (CoordsNormals objCrsnrs)
                  (Colors objColors)
                  (Indices objIndices) <- obj
    , ColoredData (CoordsNormals rcCrsnrs)
                  (Colors rcColors)
                  (Indices rcIndices) <- rcData rc
    , dataOff <- rcContentDataLength rc
    , roDataLength  <- cdVertexNum obj
    , roDataIdx     <- dataOff + 1
    , roIndexLength <- cdIndexNum obj
    , roIndexIdx    <- rcContentIndexLength rc + 1
    , ro <- RenderedObjRef {..}
    , rc' <- rc
      { rcContentDataLength = dataOff + roDataLength
      , rcNextObjId = rcNextObjId rc + 1
      , rcObjects = IntMap.insert (rcNextObjId rc) ro (rcObjects rc)
      , rcGPUData = gpuData
          { cgIdxLen = cgIdxLen gpuData + fromIntegral roIndexLength
          }
      }
      = do
    copyMutableDataFrame objCrsnrs (roDataIdx:!Z) rcCrsnrs
    copyMutableDataFrame objColors (roDataIdx:!Z) rcColors
    objIndices' <- ewmap @_ @'[] (fromIntegral dataOff +)
                <$> unsafeFreezeDataFrame objIndices
    copyDataFrame objIndices' (roIndexIdx:!Z) rcIndices
    objIndeces'' <- unsafeArrayThaw objIndices'

    -- send data to buffers
    bindBuffer gl gl_ARRAY_BUFFER (cgCoordsNormalsBuf gpuData)
    bufferSubData' gl gl_ARRAY_BUFFER (32 * dataOff) objCrsnrs

    bindBuffer gl gl_ARRAY_BUFFER (cgColorsBuf gpuData)
    bufferSubData' gl gl_ARRAY_BUFFER (4 * dataOff) objColors

    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (cgIndicesBuf gpuData)
    bufferSubData' gl gl_ELEMENT_ARRAY_BUFFER (2 * rcContentIndexLength rc) objIndeces''

    return (RenderedObjectId $ rcNextObjId rc, rc')

foreign import javascript "console.log($1);"
    js_logdf :: IODataFrame t ds -> IO ()


-- | transform object in a cell using its rendering id and tranformation matrix
transformRenderedObject :: WebGLRenderingContext
                        -> RenderingCell
                        -> RenderedObjectId
                        -> Mat44f -- ^ transformation matrix to apply on every point and normal
                        -> IO ()
transformRenderedObject gl RenderingCell{..} (RenderedObjectId i) m
    | ColoredData (CoordsNormals rcCrsnrs) _ _ <- rcData
    , Just RenderedObjRef {..} <- IntMap.lookup i rcObjects
    , Just (SomeIntNat (Proxy :: Proxy n)) <- someIntNatVal roDataLength
      = do
    -- apply matrix transform on subarray of points and normals
    objCrsnrs <- ewmap @Float @'[4] @_ @_ @Float @'[4] (m %*)
             <$> unsafeSubArrayFreeze @Float @'[4,2] @n rcCrsnrs (roDataIdx :! Z)
    copyDataFrame objCrsnrs (roDataIdx:!Z) rcCrsnrs
    objCrsnrs' <- unsafeArrayThaw objCrsnrs


    -- send data to buffers
    bindBuffer gl gl_ARRAY_BUFFER (cgCoordsNormalsBuf rcGPUData)
    bufferSubData' gl gl_ARRAY_BUFFER (32 * (roDataIdx - 1)) objCrsnrs'
-- could not lookup RenderedObjRef by its id
transformRenderedObject _ _ _ _ = return ()


-- | Fill a single object with a single color
setRenderedObjectColor :: WebGLRenderingContext
                       -> RenderingCell
                       -> RenderedObjectId
                       -> Vector GLubyte 4 -- ^ Pre-multiplied color vector @(r*a g*a b*a a)@
                       -> IO ()
setRenderedObjectColor gl RenderingCell{..} (RenderedObjectId i) c
    | ColoredData _ (Colors colors) _ <- rcData
    , Just RenderedObjRef {..} <- IntMap.lookup i rcObjects
    , Just (SomeIntNat (Proxy :: Proxy n)) <- someIntNatVal roDataLength
      = do
    -- get color subarray
    objColors <- unsafeSubArray @GLubyte @'[4] @n colors (roDataIdx :! Z)
    -- apply matrix transform on subarray of points and normals
    overDimIdx_ (dim @'[n]) (\j -> copyDataFrame c (1:!j) objColors)
    -- send data to buffers
    bindBuffer gl gl_ARRAY_BUFFER (cgColorsBuf rcGPUData)
    bufferSubData' gl gl_ARRAY_BUFFER (4 * (roDataIdx - 1)) objColors
-- could not lookup RenderedObjRef by its id
setRenderedObjectColor _ _ _ _ = return ()


-- | Delete an object from being rendered in a cell
deleteRenderedObject :: WebGLRenderingContext
                     -> RenderingCell
                     -> RenderedObjectId
                     -> IO RenderingCell
deleteRenderedObject gl rc (RenderedObjectId i)
    | Just ro@RenderedObjRef {..} <- IntMap.lookup i $ rcObjects rc
    , rcObjects'               <- IntMap.delete i $ rcObjects rc
    , ColoredData (CoordsNormals rcCrsnrs)
                  (Colors rcColors)
                  (Indices rcIndices) <- rcData rc
    , toCopyN <- max 0 $ rcContentDataLength rc + 1 - roDataLength - roDataIdx
    , toCopyM <- max 0 $ rcContentIndexLength rc + 1 - roIndexLength - roIndexIdx
    , Just (SomeIntNat (Proxy :: Proxy toCopyN)) <- someIntNatVal toCopyN
    , Just (SomeIntNat (Proxy :: Proxy toCopyM)) <- someIntNatVal toCopyM
      = if toCopyN == 0 && toCopyM == 0
        then return rc -- if the delete object was last in the list, just reduce counter
          { rcContentDataLength = roDataIdx - 1
          , rcObjects = rcObjects'
          , rcGPUData = (rcGPUData rc)
              { cgIdxLen = fromIntegral $ roIndexIdx - 1 }
          }
        else do -- if the object was in the middle of the list, we have to copy whole cell
    toCopyCrsnrs   <- unsafeSubArray @GLfloat  @'[4,2] @toCopyN rcCrsnrs  (roDataIdx + roDataLength :! Z)
    toCopyColors   <- unsafeSubArray @GLubyte  @'[4]   @toCopyN rcColors  (roDataIdx + roDataLength :! Z)
    toCopyIndices' <- ewmap @_ @'[] (flip (-) $ fromIntegral roDataLength)
                  <$> unsafeSubArrayFreeze @GLushort @'[]    @toCopyM rcIndices (roIndexIdx + roIndexLength :! Z)
    toCopyIndices  <- unsafeArrayThaw toCopyIndices'

    -- send data to buffers BEFORE doing copy within data arrays
    bindBuffer gl gl_ARRAY_BUFFER (cgCoordsNormalsBuf $ rcGPUData rc)
    bufferSubData' gl gl_ARRAY_BUFFER (32 * (roDataIdx - 1)) toCopyCrsnrs

    bindBuffer gl gl_ARRAY_BUFFER (cgColorsBuf $ rcGPUData rc)
    bufferSubData' gl gl_ARRAY_BUFFER (4 * (roDataIdx - 1)) toCopyColors

    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (cgIndicesBuf $ rcGPUData rc)
    bufferSubData' gl gl_ELEMENT_ARRAY_BUFFER (2 * (roIndexIdx - 1)) toCopyIndices

    -- update arrays
    copyMutableDataFrame toCopyCrsnrs (roDataIdx:!Z) rcCrsnrs
    copyMutableDataFrame toCopyColors (roDataIdx:!Z) rcColors
    copyMutableDataFrame toCopyIndices (roIndexIdx:!Z) rcIndices

    return $ rc
      { rcContentDataLength = rcContentDataLength rc - roDataLength
      , rcObjects = shift ro <$> rcObjects'
      , rcGPUData = (rcGPUData rc)
              { cgIdxLen = cgIdxLen (rcGPUData rc) - fromIntegral roIndexLength }
      }
  where
    shift ro ro' | roDataIdx ro' < roDataIdx ro = ro'
                 | otherwise = ro'
                    { roDataIdx = roDataIdx ro' - roDataLength ro
                    , roIndexIdx = roIndexIdx ro' - roIndexLength ro
                    }
deleteRenderedObject _ rc _ = return rc


-- | Bind drawing buffers, set up shader attributes, and draw geometry.
--   This does not include enabling vertex buffers.
renderCell :: WebGLRenderingContext -> RenderingCell -> IO ()
renderCell gl RenderingCell { rcGPUData = ColoredGeometryWebGLData {..} }
        | cgIdxLen == 0 = return ()
        | otherwise = do
    bindBuffer gl gl_ARRAY_BUFFER cgCoordsNormalsBuf >> setCoordsNormalsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER cgColorsBuf >> setColorsBuf gl
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER cgIndicesBuf
    drawElements gl gl_TRIANGLES cgIdxLen gl_UNSIGNED_SHORT 0


