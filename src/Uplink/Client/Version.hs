{-# LANGUAGE DeriveGeneric #-}
module Uplink.Client.Version where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics

data Version = Version
  { dirty   :: T.Text
  , branch  :: T.Text
  , version :: T.Text
  , commit  :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON Version
