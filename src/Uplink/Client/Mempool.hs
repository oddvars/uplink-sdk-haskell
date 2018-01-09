{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Uplink.Client.Mempool where

import Control.Monad (fail)
import Data.Aeson
import GHC.Generics

import qualified Transaction

data Mempool = Mempool
  { transactions :: [Transaction.Transaction]
  , size :: Int
  } deriving (Show, Generic)

instance FromJSON Mempool

data Size = Size Int deriving (Show)

instance FromJSON Size where
  parseJSON (Object o) = do
    v <- o .: "contents"
    return $ Size v
  parseJSON _ = fail "Cannot parse pool size"

