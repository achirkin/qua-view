{-# LANGUAGE DataKinds, FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  SmallGL.Helpers
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

module SmallGL.Helpers where

import Control.Monad (unless)

import GHCJS.WebGL

-- check if the is an error
checkGLError :: Ctx -> String -> IO ()
checkGLError gl s = do
    x <- getError gl
    unless (x == 0) . putStrLn $ "OpenGL error occured: "
        ++ s ++ " - " ++ show (toEnum . fromEnum $ x :: GLErrorCode)
-- | Errors of OpenGL calls
data GLErrorCode = GL_INVALID_ENUM | GL_INVALID_VALUE | GL_INVALID_OPERATION | GL_STACK_OVERFLOW
                 | GL_STACK_UNDERFLOW | GL_OUT_OF_MEMORY | GL_INVALID_FRAMEBUFFER_OPERATION
                 | GL_CONTEXT_LOST | GL_TABLE_TOO_LARGE | SUSSESS | UNKNOWN_CODE Int
    deriving (Eq,Show)

instance Enum GLErrorCode where
    toEnum 0 = SUSSESS
    toEnum 0x0500 = GL_INVALID_ENUM
    toEnum 0x0501 = GL_INVALID_VALUE
    toEnum 0x0502 = GL_INVALID_OPERATION
    toEnum 0x0503 = GL_STACK_OVERFLOW
    toEnum 0x0504 = GL_STACK_UNDERFLOW
    toEnum 0x0505 = GL_OUT_OF_MEMORY
    toEnum 0x0506 = GL_INVALID_FRAMEBUFFER_OPERATION
    toEnum 0x0507 = GL_CONTEXT_LOST
    toEnum 0x8031 = GL_TABLE_TOO_LARGE
    toEnum x = UNKNOWN_CODE x
    fromEnum SUSSESS = 0
    fromEnum GL_INVALID_ENUM = 0x0500
    fromEnum GL_INVALID_VALUE = 0x0501
    fromEnum GL_INVALID_OPERATION = 0x0502
    fromEnum GL_STACK_OVERFLOW = 0x0503
    fromEnum GL_STACK_UNDERFLOW = 0x0504
    fromEnum GL_OUT_OF_MEMORY = 0x0505
    fromEnum GL_INVALID_FRAMEBUFFER_OPERATION = 0x0506
    fromEnum GL_CONTEXT_LOST = 0x0507
    fromEnum GL_TABLE_TOO_LARGE = 0x8031
    fromEnum (UNKNOWN_CODE x) = x
