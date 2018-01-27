module Framework where

import System.Environment (lookupEnv)
import Data.String (IsString, fromString)
import Control.Monad (join)
import Data.Monoid ((<>))
import qualified Text.Read as Text

lookupEnvUnsafe :: IsString a => String -> IO a
lookupEnvUnsafe variable = do
  maybeVal <- lookupEnv variable
  case maybeVal of
    Nothing -> error $ "please provide environment variable " <> variable
    Just value -> return (fromString value)

lookupEnvWithDefault :: IsString a => String -> String -> IO a
lookupEnvWithDefault defaultValue variable = do
  maybeVal <- lookupEnv variable
  case maybeVal of
    Nothing -> return (fromString defaultValue)
    Just value -> return (fromString value)

-- Don't use for strings, unless your strings are wrapped in double quotes.
readEnvWithDefault :: Read a => a -> String -> IO a
readEnvWithDefault defaultValue variable = do
  maybeValue <- lookupEnv variable
  case maybeValue of
    Nothing -> return defaultValue
    Just string -> return $ Text.readMaybe string `withDefault` defaultValue

withDefault :: Maybe a -> a -> a
maybeVal `withDefault` def = case maybeVal of
  Nothing -> def
  Just val -> val
