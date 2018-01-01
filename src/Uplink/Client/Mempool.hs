{-# LANGUAGE DeriveGeneric #-}
module Uplink.Client.Mempool where

import Data.Aeson
import GHC.Generics

import qualified Transaction

data Mempool = Mempool
  { transactions :: [Transaction.Transaction]
  , size :: Int
  } deriving (Show, Generic)

instance FromJSON Mempool
