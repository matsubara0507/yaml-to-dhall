{-# LANGUAGE OverloadedLabels #-}

module YamlToDhall.Dhall
    ( toDhall
    ) where

import           RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text    as Text
import qualified RIO.Vector  as V

import           Data.Yaml   (Value (..))

toDhall :: Value -> Text
toDhall (Object hash) = hashToDhall hash
toDhall (Array arr)   = between ("[", ",", "]") $ V.toList (fmap toDhall arr)
toDhall (String txt)  = tshow txt
toDhall (Number n)    = tshow n
toDhall (Bool b)      = tshow b
toDhall Null          = "null" -- ToDo

hashToDhall :: HM.HashMap Text Value -> Text
hashToDhall =
  between ("{", ",", "}") . fmap (\(k,v) -> k <> " = " <> toDhall v) . HM.toList

between :: (Text, Text, Text) -> [Text] -> Text
between (st, sep, end) txts = mconcat [ st, Text.intercalate sep txts, end ]
