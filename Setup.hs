{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Distribution.Simple

import Distribution.PackageDescription
import Distribution.Simple.Setup
--import Distribution.Simple.Program.Types
--import Distribution.Simple.Program.Db
import Distribution.Simple.LocalBuildInfo

import Control.Arrow (second)

import Control.Monad (when)

import System.Directory
import System.Environment

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { preBuild = addBuildEnvHook $ preBuild simpleUserHooks
         , postBuild = copyOutputHook . wrapCodeHook $ postBuild simpleUserHooks
         }
-- postBuild :: Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()

wrapCodeHook :: (Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ())
             ->  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
wrapCodeHook defaultPostBuild as bf pd lbi = do
    defaultPostBuild as bf pd lbi
    readFile exeFile >>= \content -> writeFile exeFile'
      ( "var global = this;\r\nfunction runQuaView(){\r\n\"use strict\"\r\n"
       ++ content
       ++ "}\r\nwindow.onload = runQuaView.bind(this);"
      )
--    readFile exeFile >>= \content -> writeFile exeFile''
--      ( "(function(global) {\r\n\"use strict\"\r\n"
--       ++ content
--       ++ "})(typeof global !== 'undefined' ? global : this);"
--      )
  where
    exeDir =  buildDir lbi ++ "/" ++ quaView ++ "/" ++ quaView ++ ".jsexe/"
    quaView = unPackageName . pkgName $ package pd
    exeFile = exeDir ++ "/all.js"
    exeFile' = exeDir ++ "/all.wrapped.js"
--    exeFile'' = exeDir ++ "/all.forclosure.js"


copyOutputHook :: (Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ())
               ->  Args -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
copyOutputHook defaultPostBuild as bf pd lbi = do
    defaultPostBuild as bf pd lbi
    doesDirectoryExist quaServerPath >>= \e -> when e $ copyFile
        exeFile
        (quaServerPath ++ "/qua-view.js")
  where
    quaServerPath = "../qua-server/static/js"
    quaView = unPackageName . pkgName $ package pd
    exeFile = buildDir lbi ++ "/" ++ quaView ++ "/" ++ quaView ++ ".jsexe/all.wrapped.js"

addBuildEnvHook :: (Args -> BuildFlags -> IO HookedBuildInfo)
                ->  Args -> BuildFlags -> IO HookedBuildInfo
addBuildEnvHook preBuildF args bf = do
    hbi <- preBuildF args bf
    let updateBI x = x {cppOptions =  ("-DMYVAR=" ++ show "hey ho! \\ / \"cool\"!11") : cppOptions x}
    return $ second (fmap $ second updateBI) hbi
--    buildF pd lbi uh bf { buildArgs = ("-Dmyvar=" ++ show "hey ho! \\ / \"cool\"!11")  : buildArgs bf}

