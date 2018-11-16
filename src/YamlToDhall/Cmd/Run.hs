{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module YamlToDhall.Cmd.Run where

import           RIO

import           Data.Extensible
import           Data.Yaml               (decodeFileThrow)
import           YamlToDhall.Cmd.Options
import           YamlToDhall.Dhall
import           YamlToDhall.Env

run :: (MonadUnliftIO m, MonadThrow m) => Options -> m ()
run opts = do
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #logger @= logger
           <: nil
    runRIO env $ run' (listToMaybe $ opts ^. #input)

run' :: Maybe FilePath -> RIO Env ()
run' Nothing     = logError "Please input YAML file path"
run' (Just path) = do
  txt <- toDhall <$> decodeFileThrow path
  logInfo $ display txt

showNotImpl :: MonadIO m => m ()
showNotImpl = hPutBuilder stdout "not yet implement command."
