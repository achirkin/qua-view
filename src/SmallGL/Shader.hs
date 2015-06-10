-----------------------------------------------------------------------------
--
-- Module      :  SmallGL.Shader
-- Copyright   :  (c) Artem Chirkin
-- License     :  BSD3
--
-- Maintainer  :  Artem Chirkin <chirkin@arch.ethz.ch>
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module SmallGL.Shader where

import Control.Monad
import qualified Data.Map as Map

import GHCJS.WebGL

import SmallGL.Helpers

import GHCJS.Foreign
import GHCJS.Marshal
import Data.Maybe

-- | Shader program definision
data ShaderProgram = ShaderProgram {
        programId :: Program, -- ^ if of a program to supply to glUseProgram
        attributesOf :: Map.Map
            String -- ^ name of the attribute
            (GLuint, GLenum, GLint), -- ^ location,type,count
        uniformsOf :: Map.Map
            String -- ^ name of the uniform
            (UniformLocation, GLenum, GLint) -- ^ location,type,count
    }

-- | Synonym for a type of shader gl enum
type ShaderType = GLenum

-- | return attribute location by name
attrLoc :: ShaderProgram -> String -> GLuint
attrLoc program name = loc
    where (loc,_,_) = attributesOf program Map.! name

-- | return uniform location by name
unifLoc :: ShaderProgram -> String -> UniformLocation
unifLoc program name = loc
    where (loc,_,_) = uniformsOf program Map.! name

-- | Initialize shader program by supplying a number of source codes.
--   One source code per shader.
initShaders :: Ctx -> [(ShaderType, String)] -> IO ShaderProgram
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
    serror <- liftM (fromMaybe True) (getProgramParameter gl shaderProgram gl_LINK_STATUS
                >>= fromJSRef)
    unless serror $ do
        putStrLn "Shader Program linking error"
        logm <- getProgramInfoLog gl shaderProgram
        checkGLError gl "GetShaderInfoLog gl"
        putStrLn . fromJSString $ logm
    -- load attributes' information
    attrCount <- liftM (fromMaybe (0::GLuint))
        (getProgramParameter gl shaderProgram gl_ACTIVE_ATTRIBUTES >>= fromJSRef)
--    putStrLn $ "Shader attributes: " ++ show attrCount
    shaderAttribs <- liftM Map.fromList $ sequence . flip map [0..attrCount-1] $ \i -> do
        activeInfo <- getActiveAttrib gl shaderProgram i
        checkGLError gl $ "GetActiveAttrib gl for getting shader attrib" ++ show i
        ainame <- aiName activeInfo
        aisize <- aiSize activeInfo
        aitype <- aiType activeInfo
        aPos <- getAttribLocation gl shaderProgram ainame
        return (fromJSString ainame, (fromIntegral aPos, aitype, aisize))
    -- load uniforms' information
    uniCount <-  liftM (fromMaybe (0::GLuint))
        (getProgramParameter gl shaderProgram gl_ACTIVE_UNIFORMS >>= fromJSRef)
--    putStrLn $ "Shader uniforms: " ++ show attrCount
    shaderUniforms <- liftM Map.fromList $ sequence . flip map [0..uniCount-1] $ \i -> do
        activeInfo <- getActiveUniform gl shaderProgram i
        checkGLError gl $ "GetActiveUniform gl for getting shader uniform" ++ show i
        ainame <- aiName activeInfo
        aisize <- aiSize activeInfo
        aitype <- aiType activeInfo
        uPos <- getUniformLocation gl shaderProgram ainame
        return (fromJSString ainame, (uPos, aitype, aisize))
--    sequence_ . map (putStrLn . show) . Map.toList $ shaderAttribs
--    sequence_ . map (putStrLn . show) . Map.toList $ shaderUniforms
    return ShaderProgram {
            programId = shaderProgram,
            attributesOf = shaderAttribs,
            uniformsOf = shaderUniforms
        }

-- | Helper function to load shader
getShader :: Ctx -> ShaderType -> String-> IO Shader
getShader gl t src = do
    shaderId <- createShader gl t
    checkGLError gl ("CreateShader gl type " ++ show t)
    shaderSource gl shaderId (toJSString src)
    checkGLError gl ("ShaderSource gl type " ++ show t)
    compileShader gl shaderId
    checkGLError gl ("CompileShader gl type" ++ show t)
    serror <- liftM (fromMaybe True) $ getShaderParameter gl shaderId gl_COMPILE_STATUS >>= fromJSRef
    unless serror $ do
        putStrLn $ "Error in shader of type " ++ show t
        logm <- getShaderInfoLog gl shaderId
        checkGLError gl "GetShaderInfoLog gl"
        putStrLn . fromJSString $ logm
        putStrLn src
    return shaderId
