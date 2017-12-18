{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.AssetAddress where

import qualified Asset
import qualified Address
import Data.Aeson

data AssetAddress = AssetAddress
  { aaAsset :: Asset.Asset
  , aaAddress :: Address.Address
  } deriving (Show)

instance FromJSON AssetAddress where
  parseJSON = withObject "AssetAddress" $ \v -> AssetAddress
    <$> v .: "asset"
    <*> v .: "address"
