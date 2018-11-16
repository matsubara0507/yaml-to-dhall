{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module YamlToDhall.Cmd.Options where

import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt

type Options = Record
  '[ "input"   >: [String]
   , "version" >: Bool
   , "verbose" >: Bool
   , "json"    >: Bool
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""

jsonOpt :: OptDescr' Bool
jsonOpt = optFlag [] ["json"] "Convert Dhall from JSON instead of YAML"
