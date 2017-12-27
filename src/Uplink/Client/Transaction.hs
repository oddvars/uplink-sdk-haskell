{-# LANGUAGE DeriveGeneric #-}
module Uplink.Client.Transaction where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics

import qualified Address

data Transaction = Transaction
  { signature :: T.Text
  , origin    :: Address.Address
  } deriving (Show, Generic)

instance FromJSON Transaction
