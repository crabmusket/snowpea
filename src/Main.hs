module Main where

import System.Environment (getArgs)
import Data.Monoid ((<>))
import qualified Network.Wai.Handler.Warp as Warp

import Server (makeApp)
import Framework (readEnvWithDefault)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["serve"] -> do
      port <- readEnvWithDefault 3000 "PORT"
      app <- makeApp
      putStrLn ("running on port " <> show port)
      Warp.run port app
    _otherwise -> do
      putStrLn "please specify something"
