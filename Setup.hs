{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Distribution.Simple

import Distribution.PackageDescription
import Distribution.Simple.Setup
--import Distribution.Simple.Program.Types
--import Distribution.Simple.Program.Db
import Distribution.Simple.LocalBuildInfo

import Control.Monad (when)

import System.Directory

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { postBuild = copyOutputHook (postBuild simpleUserHooks)
         }
-- postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()

copyOutputHook :: (Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ())
               -> Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyOutputHook defaultPostBuild as bf pd lbi = do
    defaultPostBuild as bf pd lbi
    doesDirectoryExist quaServerPath >>= \e -> when e $ copyFile
        (buildDir lbi ++ "/" ++ quaView ++ "/" ++ quaView ++ ".jsexe/all.js")
        (quaServerPath ++ "/qua-view.js")
  where
    quaServerPath = "../qua-server/static/js"
    quaView = unPackageName . pkgName $ package pd
