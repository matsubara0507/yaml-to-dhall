{-# LANGUAGE OverloadedLabels #-}

module Lib.Cmd
    ( module X
    , Cmd (..)
    , toCmd
    ) where

import           RIO

import           Lib.Cmd.Options as X
import           Lib.Cmd.Run     as X

data Cmd
  = PrintVersion
  | RunCmd Options
  deriving (Show, Eq)

toCmd :: Options -> Cmd
toCmd opts
  | opts ^. #version = PrintVersion
  | otherwise        = RunCmd opts
