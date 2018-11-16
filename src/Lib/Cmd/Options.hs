{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

module Lib.Cmd.Options where

import           RIO

import           Data.Extensible
import           Data.Extensible.GetOpt

type Options = Record
  '[ "input"   >: [String]
   , "version" >: Bool
   , "verbose" >: Bool
   ]

versionOpt :: OptDescr' Bool
versionOpt = optFlag [] ["version"] "Show version"

verboseOpt :: OptDescr' Bool
verboseOpt = optFlag ['v'] ["verbose"] "Enable verbose mode: verbosity level \"debug\""
