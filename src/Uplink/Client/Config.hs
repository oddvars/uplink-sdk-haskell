module Uplink.Client.Config
  ( Config (..)
  ) where

import qualified Address
import qualified Key

data Config = Config
  { privateKey :: Key.PrivateKey
  , originAddress :: Address.Address
  , host :: String
  } deriving Show
