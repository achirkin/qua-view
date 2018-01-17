{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Strict #-}
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

import GHCJS.Types (JSVal)
import Data.JSString (JSString, unpack')
import Control.Monad

import JavaScript.WebGL


import SmallGL.Helpers

import GHCJS.Marshal.Pure (pFromJSVal)
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

foreign import javascript unsafe "$1 != null && $1.hasOwnProperty('location')" js_hasLoc :: JSVal -> Bool


newtype JSMap a = JSMap JSVal

jsMapFromList :: Coercible a JSVal => [(JSString, a)] -> IO (JSMap a)
jsMapFromList xs = do
    mm <- js_emptyMap
    foldM f mm xs
  where
    f m (name, val) = jssetMapVal name val m

jsIndexMap :: Coercible JSVal a => JSMap a -> JSString -> a
jsIndexMap = coerce . js_indexMap

foreign import javascript unsafe "$r = {};" js_emptyMap :: IO (JSMap a)

foreign import javascript unsafe "$r = $3; $r[$1] = $2;"
    js_setMapVal :: JSString -> JSVal -> JSMap a -> IO (JSMap a)

jssetMapVal :: Coercible a JSVal => JSString -> a -> JSMap a -> IO (JSMap a)
jssetMapVal s v = js_setMapVal s (coerce v)

foreign import javascript unsafe "$r = $1[$2];"
    js_indexMap :: JSMap a -> JSString -> JSVal


-- | Synonym for a type of shader gl enum
type ShaderType = GLenum

-- | return attribute location by name
attrLoc :: ShaderProgram -> JSString -> GLuint
attrLoc program name = let AttribProps p = attributesOf program `jsIndexMap` name
                       in if js_hasLoc p
                          then js_attrLoc $ AttribProps p
                          else error $ "Could not get attrib location: " ++ show name

-- | return uniform location by name
unifLoc :: ShaderProgram -> JSString -> WebGLUniformLocation
unifLoc program name = let UniformProps p = uniformsOf program `jsIndexMap` name
                       in if js_hasLoc p
                          then js_unifLoc $ UniformProps p
                          else error $ "Could not get uniform location: " ++ show name


-- | Initialize shader program by supplying a number of source codes.
--   One source code per shader.
initShaders :: WebGLRenderingContext -> [(ShaderType, JSString)] -> [(GLuint, JSString)] -> IO ShaderProgram
initShaders gl shtexts explicitLocs = do
    -- create program
    shaderProgram <- createProgram gl
    -- attach all shaders
    forM_ shtexts $ \(typ,text) -> do
            shader <- getShader gl typ text
            checkGLError gl $ "getShader " ++ show typ
            attachShader gl shaderProgram shader
            checkGLError gl $ "AttachShader gl " ++ show typ
    -- bind attribute locations
    forM_ explicitLocs $ \(loc,text) -> do
        bindAttribLocation gl shaderProgram loc text
        checkGLError gl $ "bindAttribLocation gl " ++ show (loc, text)
    -- check program status
    linkProgram gl shaderProgram
    checkGLError gl "link shader program"
    serror <- fromMaybe True . pFromJSVal <$> getProgramParameter gl shaderProgram gl_LINK_STATUS
    unless serror $ do
        putStrLn "Shader Program linking error "
        logm <- getProgramInfoLog gl shaderProgram
        checkGLError gl "GetShaderInfoLog gl "
        putStrLn . unpack' $ logm
    -- load attributes' information
    attrCount <- fromMaybe (0::GLuint) . pFromJSVal <$>getProgramParameter gl shaderProgram gl_ACTIVE_ATTRIBUTES
--    putStrLn $ "Shader attributes: " ++ show attrCount
    shaderAttribs <- (>>= jsMapFromList) $ sequence . flip map [0..attrCount-1] $ \i -> do
        activeInfo <- getActiveAttrib gl shaderProgram i
        checkGLError gl $ "GetActiveAttrib gl for getting shader attrib " ++ show i
        aPos <- getAttribLocation gl shaderProgram (aiName activeInfo)
        return (aiName activeInfo, js_attrProps (fromIntegral aPos) (aiType activeInfo) (aiSize activeInfo))
    -- load uniforms' information
    uniCount <- fromMaybe (0::GLuint) . pFromJSVal <$> getProgramParameter gl shaderProgram gl_ACTIVE_UNIFORMS
--    putStrLn $ "Shader uniforms: " ++ show attrCount
    shaderUniforms <- (>>= jsMapFromList) $ sequence . flip map [0..uniCount-1] $ \i -> do
        activeInfo <- getActiveUniform gl shaderProgram i
        checkGLError gl $ "GetActiveUniform gl for getting shader uniform " ++ show i
        uPos <- getUniformLocation gl shaderProgram (aiName activeInfo)
        return (aiName activeInfo, js_unifProps uPos (aiType activeInfo) (aiSize activeInfo))
    return ShaderProgram {
            programId = shaderProgram,
            attributesOf = shaderAttribs,
            uniformsOf = shaderUniforms
        }

-- | Helper function to load shader
getShader :: WebGLRenderingContext -> ShaderType -> JSString-> IO WebGLShader
getShader gl t src = do
    shaderId <- createShader gl t
    checkGLError gl ("CreateShader gl type " ++ show t)
    shaderSource gl shaderId src
    checkGLError gl ("ShaderSource gl type " ++ show t)
    compileShader gl shaderId
    checkGLError gl ("CompileShader gl type" ++ show t)
    serror <- fromMaybe True . pFromJSVal <$> getShaderParameter gl shaderId gl_COMPILE_STATUS
    unless serror $ do
        putStrLn $ "Error in shader of type " ++ show t
        logm <- getShaderInfoLog gl shaderId
        checkGLError gl "GetShaderInfoLog gl"
        putStrLn . unpack' $ logm
        putStrLn $ unpack' src
    return shaderId
