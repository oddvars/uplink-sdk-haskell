{-# LANGUAGE DeriveGeneric #-}
module Uplink.Client.Contract where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

import qualified Address
import qualified Time


data Contract = Contract
  { script    :: T.Text
  , state     :: T.Text
  , address   :: Address.Address
  , owner     :: Address.Address
  , timestamp :: Time.Timestamp
  , storage   :: Object -- todo: storage
  }
  deriving (Show, Generic)

instance FromJSON Contract
