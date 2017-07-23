{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Control.Monad (when, unless, join, forM_, (>=>))
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import System.Directory
import System.FilePath
import System.IO


main :: IO ()
main = defaultMainWithHooks simpleUserHooks
         { preBuild = addBuildEnvHook
         , postBuild = \as bf pd lbi -> do
              postBuild simpleUserHooks as bf pd lbi
              wrapCodeHook lbi
              aggregateCssHook lbi
              copyOutputHook lbi
         }


-- | It's just a simple way to get the main executable name.
myExeName :: String
myExeName = "qua-view"

-- | Here is the place where we store
makeCssGenPath :: FilePath -> FilePath
makeCssGenPath buildD = buildD </> myExeName </> (myExeName ++ "-tmp") </> "CssGen"

-- | Where all generated js code is stored
makeExeDir :: FilePath -> FilePath
makeExeDir buildD = buildD </> myExeName </> (myExeName ++ ".jsexe")

-- | A path to a special file.
--   This file contains a list of modules using TH unique identifiers.
--   That is, it keeps short unique ids for each module that needs them.
makeModulesUniquePath :: FilePath -> FilePath
makeModulesUniquePath buildD = buildD </> myExeName </> (myExeName ++ "-tmp") </> "modules.unique"

-- | Wrapp all JS code into a single function that runs after the page is loaded.
wrapCodeHook :: LocalBuildInfo -> IO ()
wrapCodeHook lbi = readFile exeFile >>= \content -> writeFile exeFile' $ unlines
    [ "var global = this;"
    , "function h$runQuaView(){"
    , "\"use strict\""
    , content
    , "}"
    , "window.onload = h$runQuaView.bind(this);"
    ]
  where
    exeDir =  makeExeDir $ buildDir lbi
    exeFile = exeDir </> "all.js"
    exeFile' = exeDir </> myExeName <.> "js"


-- | If we have the qua-sever folder near the qua-view folder (i.e. in the qua-kit repo),
--   then copy generate javascript and css files there.
copyOutputHook :: LocalBuildInfo -> IO ()
copyOutputHook lbi = doesDirectoryExist quaServerPath >>= \e -> when e $ do
    copyFile jsFile  (quaServerPath </> "js"  </> myExeName <.> "js")
    copyFile cssFile (quaServerPath </> "css" </> myExeName <.> "css")
  where
    quaServerPath = ".." </> "qua-server" </> "static"
    jsFile = makeExeDir (buildDir lbi) </> myExeName <.> "js"
    cssFile = makeExeDir (buildDir lbi) </> myExeName <.> "css"

-- | Get all files from CssGen folder and put their content into a single css file qua-view.css
aggregateCssHook :: LocalBuildInfo -> IO ()
aggregateCssHook lbi = do
    buildPrefix <- canonicalizePath (buildDir lbi) >>= makeAbsolute
    let cssGenDir = makeCssGenPath buildPrefix
        exeDir    = makeExeDir buildPrefix
        cssFile   = exeDir </> myExeName <.> "css"
        filterExistingCss fname = do
            let fpath = cssGenDir </> fname
            exists <- doesFileExist fpath
            return [fpath | exists]
    createDirectoryIfMissing True cssGenDir
    createDirectoryIfMissing True exeDir
    -- get list of css files to aggregate
    cssFiles <- listDirectory cssGenDir >>= fmap join . mapM filterExistingCss
    -- remove final css result file if exists
    doesFileExist cssFile >>= flip when (removeFile cssFile)
    -- finally write content of all css files into the new one
    withFile cssFile WriteMode $ \h -> forM_ cssFiles (readFile >=> hPutStrLn h)


-- | This function is executed before building the project.
--   Its purpose is to prepare CPP variables containing project's build folder and special files,
--   and also create them if necessary.
addBuildEnvHook :: Args -> BuildFlags -> IO HookedBuildInfo
addBuildEnvHook _ bf = do
    buildPrefix <- (>>= makeAbsolute)
                                . canonicalizePath
                                . ( </> "build")
                                . fromFlagOrDefault (error "Setup.hs/PreBuildHook: no build prefix specified!")
                                $ buildDistPref bf
    let cssGenPath    = makeCssGenPath buildPrefix
        modulesUnique = makeModulesUniquePath buildPrefix
    createDirectoryIfMissing True cssGenPath
    doesFileExist modulesUnique >>= flip unless (writeFile modulesUnique "")
    return ( Nothing
           , [( myExeName
              , emptyBuildInfo { cppOptions = [ "-DCSS_GEN_PATH=" ++ show cssGenPath
                                              , "-DMODULES_UNIQUE_PATH=" ++ show modulesUnique
                                              ]}
              )]
           )

