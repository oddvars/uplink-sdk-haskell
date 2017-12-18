{-# LANGUAGE DeriveGeneric #-}
module Uplink.Client.Op where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics

data Op = OperationOk T.Text | OperationFailed T.Text deriving (Show, Generic)

instance ToJSON   Op
instance FromJSON Op
