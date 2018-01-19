module Main ( main ) where


import Commons.NoReflex

import Workers
import Workers.LoadGeometry

main :: IO ()
main = runReaderT (execWorkerConduit loadGeometryDef loadGeometryConduit) stdOutLogger


