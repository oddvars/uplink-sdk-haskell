{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
module Uplink.Client.Contract where

import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Text as T
import GHC.Generics

import qualified Address
import qualified Time


data Contract = Contract
  { script            :: T.Text
  , state             :: T.Text
  , contractAddress   :: Address.Address
  , owner             :: Address.Address
  , contractTimestamp :: Time.Timestamp
  , storage           :: Object -- todo: storage
  }
  deriving (Show, Generic)

instance FromJSON Contract where
  parseJSON (Object v) = do
    script            <- v .: "script"
    state             <- v .: "state"
    contractAddress   <- v .: "address"
    owner             <- v .: "owner"
    contractTimestamp <- v .: "timestamp"
    storage           <- v .: "storage"
    pure Contract{..}

  parseJSON invalid  = typeMismatch "Contract" invalid
