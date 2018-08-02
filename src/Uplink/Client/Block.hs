{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.Block where

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import GHC.Generics

import qualified Address as A
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

  parseJSON invalid = typeMismatch "Block" invalid

data BlockHeader = BlockHeader
  { origin :: A.Address A.AAccount
  , prevHash :: T.Text
  , merkleRoot :: T.Text
  , timestamp :: Time.Timestamp
  } deriving (Show, Generic)

instance FromJSON BlockHeader

data BlockSignature = BlockSignature
  { signature :: T.Text
  , signerAddr :: A.Address A.AAccount
  } deriving (Show, Generic)

instance FromJSON BlockSignature
