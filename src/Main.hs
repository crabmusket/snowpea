module Main where

import Data.Monoid ((<>))
import System.Environment (getArgs)
import Network.Wai.Handler.Warp (run)

import Server (makeApp)
import Utils (readEnvWithDefault)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["serve"] -> do
      port <- readEnvWithDefault 3000 "PORT"
      app <- makeApp
      putStrLn ("running on port " <> show port)
      run port app
    _otherwise -> do
      putStrLn "please specify something"
