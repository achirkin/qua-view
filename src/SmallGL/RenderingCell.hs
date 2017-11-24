{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
module SmallGL.RenderingCell where



import JavaScript.WebGL

import Numeric.DataFrame
import Numeric.DataFrame.IO
import Numeric.Dimensions
import Numeric.Dimensions.Traverse.IO
import Numeric.TypeLits
import Commons.NoReflex.EasyTensorJSFFI

import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Unsafe.Coerce

--import Commons
import SmallGL.Types
--import SmallGL.Shader



-- | Coollection of objects in a small area packed in a single set of WebGL buffers for drawing.
data RenderingCell m = RenderingCell
  { rcData    :: !(RenderingData m)
    -- ^ All the data on host that might be needed to setup webGL buffers
  , rcGPUData :: !(WebGLRenderingData m)
    -- ^ All the WebGL buffers information
  , rcObjects :: !(IntMap RenderedObjRef)
    -- ^ Collection of the rendered objects, so that we can update, add, or delete objects dynamically.
  , rcContentDataLength  :: !Int
    -- ^ How many vertices are stored in the RenderingCell already
  , rcNextObjId :: !Int
  }

-- | How many indices are stored in the RenderingCell already
rcContentIndexLength :: RenderingCell m -> Int
rcContentIndexLength = fromIntegral . wglSeqLen . rcGPUData

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


class RenderingCells m where

    -- | Build empty rendering cell with some arrays pre-allocated
    initRenderingCell' :: forall (n :: Nat) (k :: Nat)
                       . (KnownDim n, KnownDim k)
                        => Proxy n -> Proxy k
                        -> WebGLRenderingContext -> IO (RenderingCell m)

    -- | Make data arrays twice larger than they are atm.
    growRenderingCellData :: WebGLRenderingContext -> RenderingCell m -> IO (RenderingCell m)

    -- | Make index arrays twice larger than they are atm.
    growRenderingCellIndices :: WebGLRenderingContext -> RenderingCell m -> IO (RenderingCell m)

    -- | Add one more object to the cell
    addRenderedObject :: WebGLRenderingContext
                      -> ObjRenderingData m
                      -> RenderingCell m
                      -> IO (RenderedObjectId, RenderingCell m)

    -- | transform object in a cell using its rendering id and tranformation matrix
    transformRenderedObject :: WebGLRenderingContext
                            -> RenderingCell m
                            -> RenderedObjectId
                            -> Mat44f -- ^ transformation matrix to apply on every point and normal
                            -> IO ()

    -- | transform object in a cell using its rendering id, tranformation matrix, and external source array
    transformRenderedObject' :: WebGLRenderingContext
                             -> RenderingCell m
                             -> RenderedObjectId
                             -> SomeIODataFrame Float '[N 4, XN 0]
                             -> Mat44f -- ^ transformation matrix to apply on every point and normal
                             -> IO ()

    -- | Get geometry of an object as it is stored in WebGL buffer
    copyObjectGeometry :: RenderingCell m
                       -> RenderedObjectId
                       -> IO (Maybe (SomeIODataFrame Float '[N 4, XN 0]))

    -- | Fill a single object with a single color
    setRenderedObjectColor :: WebGLRenderingContext
                           -> RenderingCell m
                           -> RenderedObjectId
                           -> Vector GLubyte 4 -- ^ Pre-multiplied color vector @(r*a g*a b*a a)@
                           -> IO ()

    -- | Delete an object from being rendered in a cell
    deleteRenderedObject :: WebGLRenderingContext
                         -> RenderingCell m
                         -> RenderedObjectId
                         -> IO (RenderingCell m)


-- | Build default empty rendering cell with some arrays pre-allocated
initRenderingCell :: RenderingCells m => WebGLRenderingContext -> IO (RenderingCell m)
initRenderingCell = initRenderingCell' (Proxy @InitialVertexSize) (Proxy @InitialIndexSize)

instance RenderingCells 'ModeColored where

    initRenderingCell' (_::Proxy n) (_::Proxy k) gl = do
        crsnrs <- newDataFrame @_ @'[4,2,n]
        colors <- newDataFrame @_ @'[4,n]
        selIds <- newDataFrame @_ @'[n]
        ixs    <- newDataFrame @_ @'[k]

        let cgIdxLen = 0
        -- create device buffers
        cgCoordsNormalsBuf <- createBuffer gl
        cgColorsBuf        <- createBuffer gl
        cgSelectIdsBuf     <- createBuffer gl
        cgIndicesBuf       <- createBuffer gl

        -- send data to buffers
        bindBuffer gl gl_ARRAY_BUFFER $ Just cgCoordsNormalsBuf
        bufferData' gl gl_ARRAY_BUFFER crsnrs gl_STATIC_DRAW

        bindBuffer gl gl_ARRAY_BUFFER $ Just cgColorsBuf
        bufferData' gl gl_ARRAY_BUFFER colors gl_STATIC_DRAW

        bindBuffer gl gl_ARRAY_BUFFER $ Just cgSelectIdsBuf
        bufferData' gl gl_ARRAY_BUFFER selIds gl_STATIC_DRAW

        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER $ Just cgIndicesBuf
        bufferData' gl gl_ELEMENT_ARRAY_BUFFER ixs gl_STATIC_DRAW

        let rcGPUData = WebGLColoredData cgIdxLen cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf
            rcData  = ColoredData (CoordsNormals crsnrs) (Colors colors) (SelectIds selIds) (Indices ixs)
            rcObjects = IntMap.empty
            rcContentDataLength = 0
            rcNextObjId = 0

        return RenderingCell {..}




    growRenderingCellData gl rc@RenderingCell {..}
        | ColoredData (CoordsNormals crsnrs)
                      (Colors (colors :: IODataFrame GLubyte '[4,n]))
                      (SelectIds selectIds)
                      ixs <- rcData
        , WebGLColoredData cgIdxLen cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf
                   <- rcGPUData
        , Evidence <- inferTimesKnownDim @2 @n
        = do
        -- cleanup existing webgl buffers
        bindBuffer gl gl_ARRAY_BUFFER Nothing
        deleteBuffer gl cgCoordsNormalsBuf
        deleteBuffer gl cgColorsBuf
        deleteBuffer gl cgSelectIdsBuf
        -- create new data buffers
        crsnrs' <- newDataFrame @_ @'[4,2,2*n]
        colors' <- newDataFrame @_ @'[4,2*n]
        selectIds' <- newDataFrame @_ @'[2*n]

        -- copy old data
        copyMutableDataFrame crsnrs (1:!Z) crsnrs'
        copyMutableDataFrame colors (1:!Z) colors'
        copyMutableDataFrame selectIds (1:!Z) selectIds'

        -- create new WebGL buffers
        cgCoordsNormalsBuf' <- createBuffer gl
        cgColorsBuf'        <- createBuffer gl
        cgSelectIdsBuf'     <- createBuffer gl

        -- send data to buffers
        bindBuffer gl gl_ARRAY_BUFFER $ Just cgCoordsNormalsBuf'
        bufferData' gl gl_ARRAY_BUFFER crsnrs' gl_STATIC_DRAW

        bindBuffer gl gl_ARRAY_BUFFER $ Just cgColorsBuf'
        bufferData' gl gl_ARRAY_BUFFER colors' gl_STATIC_DRAW

        bindBuffer gl gl_ARRAY_BUFFER $ Just cgSelectIdsBuf'
        bufferData' gl gl_ARRAY_BUFFER selectIds' gl_STATIC_DRAW

        return rc
          { rcData = ColoredData (CoordsNormals crsnrs') (Colors colors') (SelectIds selectIds') ixs
          , rcGPUData = WebGLColoredData cgIdxLen cgCoordsNormalsBuf' cgColorsBuf' cgSelectIdsBuf' cgIndicesBuf
          }


    growRenderingCellIndices gl rc@RenderingCell {..}
        | ColoredData crsnrs
                      colors
                      selectIds
                      (Indices (ixs :: IODataFrame GLushort '[m])) <- rcData
        , WebGLColoredData cgIdxLen cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf
                   <- rcGPUData
        , Evidence <- inferTimesKnownDim @2 @m
        = do
        -- cleanup existing webgl buffers
        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER Nothing
        deleteBuffer gl cgIndicesBuf
        -- create new data buffers
        ixs' <- newDataFrame @_ @'[2*m]

        -- copy old data
        copyMutableDataFrame ixs (1:!Z) ixs'

        -- create new WebGL buffers
        cgIndicesBuf' <- createBuffer gl

        -- send data to buffers
        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER $ Just cgIndicesBuf'
        bufferData' gl gl_ELEMENT_ARRAY_BUFFER ixs' gl_STATIC_DRAW

        return rc
          { rcData = ColoredData crsnrs colors selectIds (Indices ixs')
          , rcGPUData = WebGLColoredData cgIdxLen cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf'
          }



    addRenderedObject gl obj rc
        | ordVertexNum obj + rcContentDataLength rc > rdVertexNum (rcData rc)
          = growRenderingCellData gl rc >>= addRenderedObject gl obj
        | ordIndexNum obj + rcContentIndexLength rc > rdIndexNum (rcData rc)
          = growRenderingCellIndices gl rc >>= addRenderedObject gl obj
        | ObjColoredData (CoordsNormals objCrsnrs)
                         (Colors (objColors :: IODataFrame GLubyte '[4,n]))
                          selectId
                         (Indices objIndices) <- obj
        , ColoredData (CoordsNormals rcCrsnrs)
                      (Colors rcColors)
                      (SelectIds rcSelectIds)
                      (Indices rcIndices) <- rcData rc
        , WebGLColoredData cgIdxLen cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf
                  <- rcGPUData rc
        , dataOff <- rcContentDataLength rc
        , roDataLength  <- ordVertexNum obj
        , roDataIdx     <- dataOff + 1
        , roIndexLength <- ordIndexNum obj
        , roIndexIdx    <- rcContentIndexLength rc + 1
        , ro <- RenderedObjRef {..}
        , rc' <- rc
          { rcContentDataLength = dataOff + roDataLength
          , rcNextObjId = rcNextObjId rc + 1
          , rcObjects = IntMap.insert (rcNextObjId rc) ro (rcObjects rc)
          , rcGPUData = WebGLColoredData (cgIdxLen + fromIntegral roIndexLength)
                                         cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf
          }
          = do
        copyMutableDataFrame objCrsnrs (roDataIdx:!Z) rcCrsnrs
        copyMutableDataFrame objColors (roDataIdx:!Z) rcColors
        objSelectIds <- thawDataFrame . fromScalar $ scalar selectId :: IO (IODataFrame GLuint '[n])
        copyMutableDataFrame objSelectIds (roDataIdx:!Z) rcSelectIds
        objIndices' <- ewmap @_ @'[] (fromIntegral dataOff +)
                    <$> unsafeFreezeDataFrame objIndices
        copyDataFrame objIndices' (roIndexIdx:!Z) rcIndices
        objIndeces'' <- unsafeArrayThaw objIndices'

        -- send data to buffers
        bindBuffer gl gl_ARRAY_BUFFER $ Just cgCoordsNormalsBuf
        bufferSubData' gl gl_ARRAY_BUFFER (32 * dataOff) objCrsnrs

        bindBuffer gl gl_ARRAY_BUFFER $ Just cgColorsBuf
        bufferSubData' gl gl_ARRAY_BUFFER (4 * dataOff) objColors

        bindBuffer gl gl_ARRAY_BUFFER $ Just cgSelectIdsBuf
        bufferSubData' gl gl_ARRAY_BUFFER (4 * dataOff) objSelectIds

        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER $ Just cgIndicesBuf
        bufferSubData' gl gl_ELEMENT_ARRAY_BUFFER (2 * rcContentIndexLength rc) objIndeces''

        return (RenderedObjectId $ rcNextObjId rc, rc')
    addRenderedObject _ _ _ = error "add rendering object: impossible case!"


    transformRenderedObject gl RenderingCell{..} (RenderedObjectId i) m
        | ColoredData (CoordsNormals rcCrsnrs) _ _ _ <- rcData
        , Just RenderedObjRef {..} <- IntMap.lookup i rcObjects
        , Just (SomeIntNat (Proxy :: Proxy n)) <- someIntNatVal roDataLength
          = do
        -- apply matrix transform on subarray of points and normals
        objCrsnrs <- ewmap @Float @'[4] @_ @_ @Float @'[4] (m %*)
                 <$> unsafeSubArrayFreeze @Float @'[4,2] @n rcCrsnrs (roDataIdx :! Z)
        copyDataFrame objCrsnrs (roDataIdx:!Z) rcCrsnrs
        objCrsnrs' <- unsafeArrayThaw objCrsnrs

        -- send data to buffers
        bindBuffer gl gl_ARRAY_BUFFER . Just $ wglCoordsNormalsBuf rcGPUData
        bufferSubData' gl gl_ARRAY_BUFFER (32 * (roDataIdx - 1)) objCrsnrs'
    -- could not lookup RenderedObjRef by its id
    transformRenderedObject _ _ _ _ = return ()

    copyObjectGeometry RenderingCell{..} (RenderedObjectId i)
      | ColoredData (CoordsNormals rcCrsnrs) _ _ _ <- rcData
      , Just RenderedObjRef {..} <- IntMap.lookup i rcObjects
      , Just (SomeIntNat (Proxy :: Proxy n)) <- someIntNatVal roDataLength
      , Just (SomeIntNat (Proxy :: Proxy n2)) <- someIntNatVal (roDataLength*2)
        = do
      objCrsnrs <- unsafeSubArrayFreeze @Float @'[4,2] @n rcCrsnrs (roDataIdx :! Z)
      ioObjCrsnrs <- thawDataFrame (unsafeCoerce objCrsnrs :: DataFrame Float '[4,n2])
      return $ Just $ SomeIODataFrame ioObjCrsnrs
    copyObjectGeometry _ _ = pure Nothing


    transformRenderedObject' gl RenderingCell{..} (RenderedObjectId i)
                            (SomeIODataFrame (objCrsnrs :: IODataFrame Float xns)) m
        | ColoredData (CoordsNormals rcCrsnrs) _ _ _ <- rcData
        , Evidence <- unsafeCoerce (Evidence @(xns~xns)) :: Evidence ('[4,n2] ~ xns)
        , Just RenderedObjRef {..} <- IntMap.lookup i rcObjects
        , Just (SomeIntNat (Proxy :: Proxy n)) <- someIntNatVal roDataLength
          = do
        -- apply matrix transform on subarray of points and normals
        objCrsnrs' <- unsafeSubArray @Float @'[4,2] @n rcCrsnrs (roDataIdx :! Z)
        applyTransformDF m objCrsnrs objCrsnrs'

        -- send data to buffers
        bindBuffer gl gl_ARRAY_BUFFER . Just $ wglCoordsNormalsBuf rcGPUData
        bufferSubData' gl gl_ARRAY_BUFFER (32 * (roDataIdx - 1)) objCrsnrs'
    -- could not lookup RenderedObjRef by its id
    transformRenderedObject' _ _ _ _ _ = return ()




    setRenderedObjectColor gl RenderingCell{..} (RenderedObjectId i) c
        | ColoredData _ (Colors colors) _ _ <- rcData
        , Just RenderedObjRef {..} <- IntMap.lookup i rcObjects
        , Just (SomeIntNat (Proxy :: Proxy n)) <- someIntNatVal roDataLength
          = do
        -- get color subarray
        objColors <- unsafeSubArray @GLubyte @'[4] @n colors (roDataIdx :! Z)
        -- apply matrix transform on subarray of points and normals
        overDimIdx_ (dim @'[n]) (\j -> copyDataFrame c (1:!j) objColors)
        -- send data to buffers
        bindBuffer gl gl_ARRAY_BUFFER . Just $ wglColorsBuf rcGPUData
        bufferSubData' gl gl_ARRAY_BUFFER (4 * (roDataIdx - 1)) objColors
    -- could not lookup RenderedObjRef by its id
    setRenderedObjectColor _ _ _ _ = return ()



    deleteRenderedObject gl rc (RenderedObjectId i)
        | Just ro@RenderedObjRef {..} <- IntMap.lookup i $ rcObjects rc
        , rcObjects'               <- IntMap.delete i $ rcObjects rc
        , ColoredData (CoordsNormals rcCrsnrs)
                      (Colors rcColors)
                      (SelectIds rcSelIds)
                      (Indices rcIndices) <- rcData rc
        , WebGLColoredData cgIdxLen cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf
                  <- rcGPUData rc
        , toCopyN <- max 0 $ rcContentDataLength rc + 1 - roDataLength - roDataIdx
        , toCopyM <- max 0 $ rcContentIndexLength rc + 1 - roIndexLength - roIndexIdx
        , Just (SomeIntNat (Proxy :: Proxy toCopyN)) <- someIntNatVal toCopyN
        , Just (SomeIntNat (Proxy :: Proxy toCopyM)) <- someIntNatVal toCopyM
          = if toCopyN == 0 && toCopyM == 0
            then return rc -- if the delete object was last in the list, just reduce counter
              { rcContentDataLength = roDataIdx - 1
              , rcObjects = rcObjects'
              , rcGPUData = WebGLColoredData (fromIntegral $ roIndexIdx - 1)
                                             cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf
              }
            else do -- if the object was in the middle of the list, we have to copy whole cell
        toCopyCrsnrs   <- unsafeSubArray @GLfloat  @'[4,2] @toCopyN rcCrsnrs (roDataIdx + roDataLength :! Z)
        toCopyColors   <- unsafeSubArray @GLubyte  @'[4]   @toCopyN rcColors (roDataIdx + roDataLength :! Z)
        toCopySelIds   <- unsafeSubArray @GLuint   @'[]    @toCopyN rcSelIds (roDataIdx + roDataLength :! Z)
        toCopyIndices' <- ewmap @_ @'[] (flip (-) $ fromIntegral roDataLength)
                      <$> unsafeSubArrayFreeze @GLushort @'[]    @toCopyM rcIndices (roIndexIdx + roIndexLength :! Z)
        toCopyIndices  <- unsafeArrayThaw toCopyIndices'

        -- send data to buffers BEFORE doing copy within data arrays
        bindBuffer gl gl_ARRAY_BUFFER $ Just cgCoordsNormalsBuf
        bufferSubData' gl gl_ARRAY_BUFFER (32 * (roDataIdx - 1)) toCopyCrsnrs

        bindBuffer gl gl_ARRAY_BUFFER $ Just cgColorsBuf
        bufferSubData' gl gl_ARRAY_BUFFER (4 * (roDataIdx - 1)) toCopyColors

        bindBuffer gl gl_ARRAY_BUFFER $ Just cgSelectIdsBuf
        bufferSubData' gl gl_ARRAY_BUFFER (4 * (roDataIdx - 1)) toCopySelIds

        bindBuffer gl gl_ELEMENT_ARRAY_BUFFER $ Just cgIndicesBuf
        bufferSubData' gl gl_ELEMENT_ARRAY_BUFFER (2 * (roIndexIdx - 1)) toCopyIndices

        -- update arrays
        copyMutableDataFrame toCopyCrsnrs (roDataIdx:!Z) rcCrsnrs
        copyMutableDataFrame toCopyColors (roDataIdx:!Z) rcColors
        copyMutableDataFrame toCopySelIds (roDataIdx:!Z) rcSelIds
        copyMutableDataFrame toCopyIndices (roIndexIdx:!Z) rcIndices

        return $ rc
          { rcContentDataLength = rcContentDataLength rc - roDataLength
          , rcObjects = shift ro <$> rcObjects'
          , rcGPUData = WebGLColoredData (cgIdxLen - fromIntegral roIndexLength)
                                          cgCoordsNormalsBuf cgColorsBuf cgSelectIdsBuf cgIndicesBuf
          }
      where
        shift ro ro' | roDataIdx ro' < roDataIdx ro = ro'
                     | otherwise = ro'
                        { roDataIdx = roDataIdx ro' - roDataLength ro
                        , roIndexIdx = roIndexIdx ro' - roIndexLength ro
                        }
    deleteRenderedObject _ rc _ = return rc


deleteRenderingCell :: WebGLRenderingContext -> RenderingCell m -> IO ()
deleteRenderingCell gl RenderingCell{ rcGPUData = WebGLPointData _ a b c }
  = deleteBuffer gl a >> deleteBuffer gl b >> deleteBuffer gl c
deleteRenderingCell gl RenderingCell{ rcGPUData = WebGLLineData _ a b c d }
  = deleteBuffer gl a >> deleteBuffer gl b >> deleteBuffer gl c >> deleteBuffer gl d
deleteRenderingCell gl RenderingCell{ rcGPUData = WebGLColoredData _ a b c d }
  = deleteBuffer gl a >> deleteBuffer gl b >> deleteBuffer gl c >> deleteBuffer gl d
deleteRenderingCell gl RenderingCell{ rcGPUData = WebGLTexturedData _ a b c d }
  = deleteBuffer gl a >> deleteBuffer gl b >> deleteBuffer gl c >> deleteBuffer gl d



-- | Bind drawing buffers, set up shader attributes, and draw geometry.
--   This does not include enabling vertex buffers.
renderCell :: WebGLRenderingContext -> RenderingCell m -> IO ()
renderCell _  rc | wglSeqLen (rcGPUData rc) == 0 = return ()
renderCell gl RenderingCell { rcGPUData = d@WebGLPointData {} } = do
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglCoordsBuf d) >> setCoordsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglColorsBuf d) >> setColorsBuf gl
    drawArrays gl gl_POINTS 0 (wglSeqLen d)
renderCell gl RenderingCell { rcGPUData = d@WebGLLineData {} } = do
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglCoordsBuf d) >> setCoordsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglColorsBuf d) >> setColorsBuf gl
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (Just $ wglIndicesBuf d)
    drawElements gl gl_LINES (wglSeqLen d) gl_UNSIGNED_SHORT 0
renderCell gl RenderingCell { rcGPUData = d@WebGLColoredData {} } = do
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglCoordsNormalsBuf d) >> setCoordsNormalsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglColorsBuf d) >> setColorsBuf gl
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (Just $ wglIndicesBuf d)
    drawElements gl gl_TRIANGLES (wglSeqLen d) gl_UNSIGNED_SHORT 0
renderCell gl RenderingCell { rcGPUData = d@WebGLTexturedData {} } = do
    -- TODO need a texture!
    -- bindTexture gl gl_TEXTURE_2D gmcMapTexture
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglCoordsNormalsBuf d) >> setCoordsNormalsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglTexCoordsBuf d)     >> setTexCoordsBuf gl
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (Just $ wglIndicesBuf d)
    drawElements gl gl_TRIANGLES (wglSeqLen d) gl_UNSIGNED_SHORT 0


-- | Render ids of objects into a supplied framebuffer.
--   Then we can use it to find out which object is selected by their ids.
renderCellSelectors :: WebGLRenderingContext -> RenderingCell m -> IO ()
renderCellSelectors _ rc | wglSeqLen (rcGPUData rc) == 0 = return ()
renderCellSelectors gl RenderingCell { rcGPUData = d@WebGLPointData {} } = do
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglCoordsBuf d) >> setCoordsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglSelectorIdsBuf d) >> setSelIdsBuf gl
    drawArrays gl gl_POINTS 0 (wglSeqLen d)
renderCellSelectors gl RenderingCell { rcGPUData = d@WebGLLineData {} } = do
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglCoordsBuf d) >> setCoordsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglSelectorIdsBuf d) >> setSelIdsBuf gl
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (Just $ wglIndicesBuf d)
    drawElements gl gl_LINES (wglSeqLen d) gl_UNSIGNED_SHORT 0
renderCellSelectors gl RenderingCell { rcGPUData =  d@WebGLColoredData {}  } = do
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglCoordsNormalsBuf d) >> setCoordsNoNormalsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglSelectorIdsBuf d) >> setSelIdsBuf gl
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (Just $ wglIndicesBuf d)
    drawElements gl gl_TRIANGLES (wglSeqLen d) gl_UNSIGNED_SHORT 0
renderCellSelectors gl RenderingCell { rcGPUData =  d@WebGLTexturedData {}  } = do
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglCoordsNormalsBuf d) >> setCoordsNoNormalsBuf gl
    bindBuffer gl gl_ARRAY_BUFFER (Just $ wglSelectorIdsBuf d) >> setSelIdsBuf gl
    bindBuffer gl gl_ELEMENT_ARRAY_BUFFER (Just $ wglIndicesBuf d)
    drawElements gl gl_TRIANGLES (wglSeqLen d) gl_UNSIGNED_SHORT 0

