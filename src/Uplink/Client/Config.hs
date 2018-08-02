module Uplink.Client.Config
  ( Config (..)
  ) where

import qualified Address as A
import qualified Key

data Config = Config
  { privateKey :: Key.PrivateKey
  , originAddress :: A.Address A.AAccount
  , host :: String
  } deriving Show
