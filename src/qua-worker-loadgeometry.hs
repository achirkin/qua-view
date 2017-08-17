{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where


import Commons

import Workers

main :: IO ()
main = flip runReaderT stdOutLogger $ do
    logInfo @JSString "Geometry Loading Worker" "Hello world from web worker!"
    w <- getSelf
    void $ onMessage w (logInfo' @JSString "Geometry Loading Worker" "Got a message!")
