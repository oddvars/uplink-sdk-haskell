{-# LANGUAGE DeriveGeneric #-}
module Uplink.Client.Contract where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Contract = Contract
  { state :: T.Text }
  deriving (Show, Generic)

instance FromJSON Contract

