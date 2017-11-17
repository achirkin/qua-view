module Main ( main ) where


import Commons.NoReflex
import Control.Monad.Trans.State.Strict

import Workers
import Workers.LoadGeometry

main :: IO ()
main = flip evalStateT mempty
     $ runReaderT (execWorkerConduit loadGeometryDef loadGeometryConduit) stdOutLogger


