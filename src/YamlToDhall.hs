{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE MultiWayIf       #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module YamlToDhall
  ( Options
  , run
  ) where

import           RIO

import           Data.Aeson        (decodeFileStrict)
import           Data.Extensible
import           Data.Yaml         (decodeFileThrow)
import           YamlToDhall.Dhall
import           YamlToDhall.Env

type Options = Record
  '[ "input"   >: [String]
   , "help"    >: Bool
   , "version" >: Bool
   , "verbose" >: Bool
   , "json"    >: Bool
   ]

run :: MonadUnliftIO m => Options -> m ()
run opts = do
  logOpts <- logOptionsHandle stdout (opts ^. #verbose)
  withLogFunc logOpts $ \logger -> do
    let env = #logger @= logger
           <: nil
    runRIO env $ run' (listToMaybe $ opts ^. #input) (opts ^. #json)

run' :: Maybe FilePath -> Bool -> RIO Env ()
run' Nothing     _      = logError "Please input YAML file path"
run' (Just path) isJson = do
  txt <- if
    | isJson    -> liftIO $ decodeFileStrict path
    | otherwise -> pure <$> decodeFileThrow path
  case txt of
    Nothing   -> logError "Parse JSON Error"
    Just txt' -> logInfo $ display (toDhall txt')
