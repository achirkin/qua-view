{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SmallGL.RenderingMapTiles
    ( MapTile (..)
    , RenderMTilesProgram ()
    , initMapTilesProgram
    , renderMapTiles
    , addMapTile
    , clearMapTiles
    , setMapTileOpacity
    ) where


import JavaScript.WebGL

import Numeric.DataFrame
import Commons.NoReflex
import SmallGL.Types
import SmallGL.Shader


data MapTile = MapTile
    { mtVertexBuffer :: !WebGLBuffer
    , mtTexture      :: !WebGLTexture
    }


data RenderMTilesProgram
  = RenderMTilesProgram
  { rmtRenderProg      :: !RenderingProgram
  , rmtTexOpacity      :: !Scf
  , rmtTiles           :: ![MapTile]
  , rmtTexCoordsBuffer :: !WebGLBuffer
  }


setMapTileOpacity :: Scf -> RenderMTilesProgram -> RenderMTilesProgram
setMapTileOpacity o p = p { rmtTexOpacity = o}

initMapTilesProgram :: WebGLRenderingContext -> Scf -> IO RenderMTilesProgram
initMapTilesProgram gl rmtTexOpacity = do
  shader <- initShaders gl
    [(gl_VERTEX_SHADER, vertexMapShaderText)
    ,(gl_FRAGMENT_SHADER, fragmentMapShaderText)
    ]
    [(attrLocCoords, "aVertexPosition")
    ,(attrLocTexCoords,"aTextureCoord")
    ]
  let uProjLoc = unifLoc shader "uProjM"
      uViewLoc = unifLoc shader "uViewM"
      uCustomLoc3 = unifLoc shader "uSampler"
      uCustomLoc4 = unifLoc shader "uClippingDist"
      uCustomLoc5 = unifLoc shader "uTexOpacity"
      rmtRenderProg = RenderingProgram {..}
      rmtTiles = []

  rmtTexCoordsBuffer <- createBuffer gl
  texCoordsIO <- unsafeArrayThaw texCoords
  bindBuffer gl gl_ARRAY_BUFFER $ Just rmtTexCoordsBuffer
  bufferData' gl gl_ARRAY_BUFFER texCoordsIO gl_STATIC_DRAW

  return RenderMTilesProgram {..}


addMapTile :: WebGLRenderingContext
           -> DataFrame Float '[4,4] -- ^ four vertices
           -> TexImageSource
           -> RenderMTilesProgram
           -> IO RenderMTilesProgram
addMapTile gl vertices tileImg rmtProg = do

    mtVertexBuffer <- createBuffer gl
    verticesIO <- unsafeArrayThaw vertices
    bindBuffer gl gl_ARRAY_BUFFER $ Just mtVertexBuffer
    bufferData' gl gl_ARRAY_BUFFER verticesIO gl_STATIC_DRAW

    mtTexture <- createTexture gl
    bindTexture gl gl_TEXTURE_2D $ Just mtTexture
    pixelStorei gl gl_UNPACK_FLIP_Y_WEBGL 1
    texImage2DImg gl gl_TEXTURE_2D 0 gl_RGBA gl_RGBA gl_UNSIGNED_BYTE tileImg
    setTexParameters gl
    bindTexture gl gl_TEXTURE_2D Nothing

    return rmtProg { rmtTiles = MapTile {..} : rmtTiles rmtProg }

-- | Clear tiles only. Keeps the program itself
clearMapTiles :: WebGLRenderingContext
              -> RenderMTilesProgram
              -> IO RenderMTilesProgram
clearMapTiles gl rmtp = do
    bindTexture gl gl_TEXTURE_2D Nothing
    bindBuffer gl gl_ARRAY_BUFFER Nothing
    forM_ (rmtTiles rmtp) $ \MapTile{..} ->
      deleteBuffer gl mtVertexBuffer >> deleteTexture gl mtTexture
    return rmtp { rmtTiles = [] }


renderMapTiles :: WebGLRenderingContext
               -> ProjMatrix
               -> ViewMatrix
               -> RenderMTilesProgram
               -> IO ()
renderMapTiles _ _ _ rmtp | null (rmtTiles rmtp) = return ()
renderMapTiles gl uProjM uViewM RenderMTilesProgram {..}
  = do
    useProgram gl . programId $ shader rmtRenderProg
    enableCoordsBuf gl
    enableTexCoordsBuf gl
    activeTexture gl gl_TEXTURE0

    -- supply shader with uniforms
    uniformMatrix4fv gl (uProjLoc rmtRenderProg) False (getProjM uProjM)
    uniformMatrix4fv gl (uViewLoc rmtRenderProg) False (getViewM uViewM)
    uniform1fv gl (uClippingDistLoc rmtRenderProg) (projMToClippingDist uProjM)
    uniform1i  gl (uSamplerLoc      rmtRenderProg) 0
    uniform1fv gl (uTexOpacityLoc   rmtRenderProg) rmtTexOpacity

    depthMask gl False
    bindBuffer gl gl_ARRAY_BUFFER (Just rmtTexCoordsBuffer) >> setTexCoordsBuf gl
    forM_ rmtTiles $ \MapTile{..} -> do
      bindTexture gl gl_TEXTURE_2D $ Just mtTexture
      bindBuffer gl gl_ARRAY_BUFFER (Just mtVertexBuffer) >> setCoordsBuf gl
      drawArrays gl gl_TRIANGLE_STRIP 0 4
    bindTexture gl gl_TEXTURE_2D Nothing
    depthMask gl True
    disableCoordsBuf gl
    disableTexCoordsBuf gl




fragmentMapShaderText :: JSString
fragmentMapShaderText =
    [jsstring|
      precision mediump float;
      uniform sampler2D uSampler;
      uniform float uTexOpacity;
      varying vec2 vTextureCoord;
      varying vec3 vDist;
      void main(void) {
        mediump float fade = clamp(#{x} - dot(vDist,vDist), 0.0, 1.0) * uTexOpacity;
        gl_FragColor = texture2D(uSampler, vec2(vTextureCoord.s, vTextureCoord.t)) * fade;
      }
    |]
  where
   x = toJSString $ show fadeConst

vertexMapShaderText :: JSString
vertexMapShaderText =
    [jsstring|
      precision mediump float;
      attribute vec4 aVertexPosition;
      attribute vec2 aTextureCoord;
      uniform mat4 uViewM;
      uniform mat4 uProjM;
      uniform float uClippingDist;
      varying vec3 vDist;
      varying vec2 vTextureCoord;
      void main(void) {
        vec4 globalPos = uViewM * aVertexPosition;
        gl_Position = uProjM * globalPos;
        vDist = globalPos.xyz/(globalPos.w*uClippingDist*#{x});
        vTextureCoord = aTextureCoord;
      }
    |]
  where
    x = toJSString . show $ 1 / sqrt fadeConst


texCoords :: DataFrame GLushort '[2,4]
texCoords = vec2 minBound minBound
       <::> vec2 maxBound minBound
       <+:> vec2 minBound maxBound
       <+:> vec2 maxBound maxBound
