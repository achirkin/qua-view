module Main ( main ) where


import Commons

import Workers
import Workers.LoadGeometry

main :: IO ()
main = runReaderT (execWorkerConduit loadGeometryDef $ loadGeometryConduit 3) stdOutLogger


