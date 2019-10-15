{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Paths_yaml_to_dhall    (version)
import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt
import           GetOpt                 (withGetOpt')
import qualified Version
import qualified YamlToDhall

main :: IO ()
main = withGetOpt' "[options] [input-file]" opts $ \r args usage -> if
  | r ^. #help    -> hPutBuilder stdout (fromString usage)
  | r ^. #version -> hPutBuilder stdout (Version.build version)
  | otherwise     -> YamlToDhall.run (#input @= args <: r)
  where
    opts = #help    @= optFlag ['h'] ["help"] "Show this help text"
        <: #version @= optFlag [] ["version"] "Show version"
        <: #verbose @= optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""
        <: #json    @= optFlag [] ["json"] "Convert Dhall from JSON instead of YAML"
        <: nil
