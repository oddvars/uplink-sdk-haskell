{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.Block where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

import qualified Address
import qualified Time


data Block = Block
  { index :: Int
  , blockHeader :: BlockHeader
  , signatures :: [BlockSignature]
  } deriving (Show, Generic)

instance FromJSON Block where
  parseJSON (Object v) = do
    ix   <- v .: "index"
    hdr  <- v .: "header"
    sigs <- v .: "signature"
    return $ Block ix hdr sigs

data BlockHeader = BlockHeader
  { origin :: Address.Address
  , prevHash :: T.Text
  , merkleRoot :: T.Text
  , timestamp :: Time.Timestamp
  } deriving (Show, Generic)

instance FromJSON BlockHeader

data BlockSignature = BlockSignature
  { signature :: T.Text
  , signerAddr :: Address.Address
  } deriving (Show, Generic)

instance FromJSON BlockSignature
