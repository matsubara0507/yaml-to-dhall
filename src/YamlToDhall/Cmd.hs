{-# LANGUAGE OverloadedLabels #-}

module YamlToDhall.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    ) where

import           RIO

import           YamlToDhall.Cmd.Options as X
import           YamlToDhall.Cmd.Run     as X

data Cmd
  = PrintVersion
  | RunCmd Options
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version = PrintVersion
  | otherwise        = RunCmd opts
