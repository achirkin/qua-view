{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  SmallGL.Shader
-- Copyright   :  (c) Artem Chirkin
-- License     :  MIT
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
--
-----------------------------------------------------------------------------

module SmallGL.Shader
    ( ShaderProgram (), programId
    , initShaders, attrLoc, unifLoc
    ) where

import JsHs.JSString (unpack', pack)
import Control.Monad

import JsHs.WebGL
import JsHs.Types
import JsHs.LikeJS.Class

import SmallGL.Helpers

-- import GHCJS.Marshal
import Data.Maybe
import Data.Coerce

-- | Shader program definision
data ShaderProgram = ShaderProgram
    { programId :: WebGLProgram -- ^ if of a program to supply to glUseProgram
    , attributesOf :: JSMap AttribProps -- ^ name of the attribute - location,type,count
    , uniformsOf :: JSMap UniformProps -- ^ name of the uniform - location,type,count
    }


newtype AttribProps = AttribProps JSVal
newtype UniformProps = UniformProps JSVal

foreign import javascript unsafe "$1.location" js_attrLoc :: AttribProps -> GLuint
--foreign import javascript unsafe "$1.type" js_attrType :: AttribProps -> GLenum
--foreign import javascript unsafe "$1.count" js_attrCount :: AttribProps -> GLint
foreign import javascript unsafe "$r = {}; $r.location = $1; $r.type = $2; $r.count = $3;"
    js_attrProps :: GLuint -> GLenum -> GLint -> AttribProps

foreign import javascript unsafe "$1.location" js_unifLoc :: UniformProps -> WebGLUniformLocation
--foreign import javascript unsafe "$1.type" js_unifType :: UniformProps -> GLenum
--foreign import javascript unsafe "$1.count" js_unifCount :: UniformProps -> GLint
foreign import javascript unsafe "$r = {}; $r.location = $1; $r.type = $2; $r.count = $3;"
    js_unifProps :: WebGLUniformLocation -> GLenum -> GLint -> UniformProps


newtype JSMap a = JSMap JSVal

jsMapFromList :: Coercible a JSVal => [(JSString, a)] -> JSMap a
jsMapFromList xs = foldl f mm xs
    where mm = js_emptyMap
          f m (name, val) = jssetMapVal name val m

jsIndexMap :: Coercible JSVal a => JSMap a -> JSString -> a
jsIndexMap = coerce . js_indexMap

foreign import javascript unsafe "$r = {};" js_emptyMap :: JSMap a

foreign import javascript unsafe "$r = $3; $r[$1] = $2;"
    js_setMapVal :: JSString -> JSVal -> JSMap a -> JSMap a

jssetMapVal :: Coercible a JSVal => JSString -> a -> JSMap a -> JSMap a
jssetMapVal s v = js_setMapVal s (coerce v)

foreign import javascript unsafe "$r = $1[$2];"
    js_indexMap :: JSMap a -> JSString -> JSVal


-- | Synonym for a type of shader gl enum
type ShaderType = GLenum

-- | return attribute location by name
attrLoc :: ShaderProgram -> JSString -> GLuint
attrLoc program name = js_attrLoc $ attributesOf program `jsIndexMap` name

-- | return uniform location by name
unifLoc :: ShaderProgram -> JSString -> WebGLUniformLocation
unifLoc program name = js_unifLoc $ uniformsOf program `jsIndexMap` name

-- | Initialize shader program by supplying a number of source codes.
--   One source code per shader.
initShaders :: WebGLRenderingContext -> [(ShaderType, String)] -> IO ShaderProgram
initShaders gl shtexts = do
    -- create program
    shaderProgram <- createProgram gl
    -- attach all shaders
    forM_ shtexts $ \(typ,text) -> do
            shader <- getShader gl typ text
            checkGLError gl $ "getShader " ++ show typ
            attachShader gl shaderProgram shader
            checkGLError gl $ "AttachShader gl" ++ show typ
    -- check program status
    linkProgram gl shaderProgram
    checkGLError gl "link shader program"
    serror <- fromMaybe True . asLikeJS <$> getProgramParameter gl shaderProgram gl_LINK_STATUS
    unless serror $ do
        putStrLn "Shader Program linking error"
        logm <- getProgramInfoLog gl shaderProgram
        checkGLError gl "GetShaderInfoLog gl"
        putStrLn . unpack' $ logm
    -- load attributes' information
    attrCount <- fromMaybe (0::GLuint) . asLikeJS <$>getProgramParameter gl shaderProgram gl_ACTIVE_ATTRIBUTES
--    putStrLn $ "Shader attributes: " ++ show attrCount
    shaderAttribs <- liftM jsMapFromList $ sequence . flip map [0..attrCount-1] $ \i -> do
        activeInfo <- getActiveAttrib gl shaderProgram i
        checkGLError gl $ "GetActiveAttrib gl for getting shader attrib" ++ show i
        aPos <- getAttribLocation gl shaderProgram (aiName activeInfo)
        return (aiName activeInfo, js_attrProps (fromIntegral aPos) (aiType activeInfo) (aiSize activeInfo))
    -- load uniforms' information
    uniCount <- fromMaybe (0::GLuint) . asLikeJS <$> getProgramParameter gl shaderProgram gl_ACTIVE_UNIFORMS
--    putStrLn $ "Shader uniforms: " ++ show attrCount
    shaderUniforms <- liftM jsMapFromList $ sequence . flip map [0..uniCount-1] $ \i -> do
        activeInfo <- getActiveUniform gl shaderProgram i
        checkGLError gl $ "GetActiveUniform gl for getting shader uniform" ++ show i
        uPos <- getUniformLocation gl shaderProgram (aiName activeInfo)
        return (aiName activeInfo, js_unifProps uPos (aiType activeInfo) (aiSize activeInfo))
--    sequence_ . map (putStrLn . show) . Map.toList $ shaderAttribs
--    sequence_ . map (putStrLn . show) . Map.toList $ shaderUniforms
    return ShaderProgram {
            programId = shaderProgram,
            attributesOf = shaderAttribs,
            uniformsOf = shaderUniforms
        }

-- | Helper function to load shader
getShader :: WebGLRenderingContext -> ShaderType -> String-> IO WebGLShader
getShader gl t src = do
    shaderId <- createShader gl t
    checkGLError gl ("CreateShader gl type " ++ show t)
    shaderSource gl shaderId (pack src)
    checkGLError gl ("ShaderSource gl type " ++ show t)
    compileShader gl shaderId
    checkGLError gl ("CompileShader gl type" ++ show t)
    serror <- fromMaybe True . asLikeJS <$> getShaderParameter gl shaderId gl_COMPILE_STATUS
    unless serror $ do
        putStrLn $ "Error in shader of type " ++ show t
        logm <- getShaderInfoLog gl shaderId
        checkGLError gl "GetShaderInfoLog gl"
        putStrLn . unpack' $ logm
        putStrLn src
    return shaderId
