{-# LANGUAGE DeriveGeneric #-}
module Uplink.Client.Block where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

import qualified Address
import qualified Time


data Block = Block
  { index :: Int
  , header :: BlockHeader
  , signatures :: [BlockSignature]
  } deriving (Show, Generic)

instance FromJSON Block

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
