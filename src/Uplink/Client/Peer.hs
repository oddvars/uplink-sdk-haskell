{-# LANGUAGE DeriveGeneric #-}
module Uplink.Client.Peer where

import           Data.Aeson
import qualified Data.Text as T
import           GHC.Generics

data Peer = Peer
  { tag :: T.Text
  , contents :: PeerInfo
  } deriving (Show, Generic)

instance FromJSON Peer

data PeerInfo = PeerInfo
  { peerPid :: T.Text
  , peerAccAddr :: T.Text
  } deriving (Show, Generic)

instance FromJSON PeerInfo where
