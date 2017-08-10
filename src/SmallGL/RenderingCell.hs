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

import Data.IORef
import Data.Bits
import Unsafe.Coerce

import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.TypeLits

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap


import Commons
import SmallGL.Types
import SmallGL.Shader



newtype RenderedObjectId = RenderedObjectId Int
    deriving (Eq, Ord)

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
    arrayBuffer crsnrs >>= \buf ->
        bufferData gl gl_ARRAY_BUFFER buf gl_STATIC_DRAW

    bindBuffer gl gl_ARRAY_BUFFER cgColorsBuf
    arrayBuffer colors >>= \buf ->
        bufferData gl gl_ARRAY_BUFFER buf gl_STATIC_DRAW

    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER cgIndicesBuf
    arrayBuffer ixs >>= \buf ->
        bufferData gl gl_ELEMENT_ARRAY_BUFFER buf gl_STATIC_DRAW

    let rcGPUData = ColoredGeometryWebGLData {..}
        rcData  = ColoredData (CoordsNormals crsnrs) (Colors colors) (Indices ixs)
        rcObjects = IntMap.empty
        rcContentDataLength = 0

    return RenderingCell {..}


-- | Make data arrays twice larger than they are atm.
growRenderingCellData :: WebGLRenderingContext -> RenderingCell -> IO RenderingCell
growRenderingCellData gl rc@RenderingCell {..}
    | ColoredData (CoordsNormals crsnrs)
                  (Colors (colors :: IODataFrame GLubyte '[4,n]))
                  ixs <- rcData
    , Evidence <- inferTimesKnownDim @2 @n
    , Evidence <- unsafeCoerce (Evidence :: Evidence (1 <= 2)) :: Evidence (n <= 2*n)
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
    arrayBuffer crsnrs >>= \buf ->
        bufferData gl gl_ARRAY_BUFFER buf gl_STATIC_DRAW

    bindBuffer gl gl_ARRAY_BUFFER cgColorsBuf'
    arrayBuffer colors >>= \buf ->
        bufferData gl gl_ARRAY_BUFFER buf gl_STATIC_DRAW

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
    , Evidence <- unsafeCoerce (Evidence :: Evidence (1 <= 2)) :: Evidence (m <= 2*m)
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
    arrayBuffer ixs' >>= \buf ->
        bufferData gl gl_ELEMENT_ARRAY_BUFFER buf gl_STATIC_DRAW

    return rc
      { rcData = ColoredData crsnrs colors (Indices ixs')
      , rcGPUData = rcGPUData
          { cgIndicesBuf = cgIndicesBuf'
          }
      }



addRenderedObject :: ColoredData
                  -> RenderingCell
                  -> (RenderedObjectId, RenderingCell)
addRenderedObject = undefined






