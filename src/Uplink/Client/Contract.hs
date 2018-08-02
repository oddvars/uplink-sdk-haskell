{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE DeriveGeneric     #-}
module Uplink.Client.Contract where

import           Data.Aeson
import           Data.Aeson.Types (typeMismatch)
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import           GHC.Generics

import qualified Address as A
import qualified Time


data Contract = Contract
  { script            :: T.Text
  , state             :: T.Text
  , contractAddress   :: A.Address A.AAccount
  , owner             :: A.Address A.AAccount
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

type Method = M.HashMap T.Text Array
