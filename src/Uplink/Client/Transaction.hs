{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Uplink.Client.Transaction
  ( Transaction (..)
  , TransactionHeader (..)
  , InvalidTransaction (..)
  , Tx.TxAsset (..)
  , Tx.TxAccount (..)
  , TxContract (..)
  ) where

import           Data.Aeson hiding (encode, decode)
import           Data.Aeson.Types
import qualified Data.ByteString as BS
import qualified Data.Serialize as S
import           GHC.Generics
import           Protolude

import qualified Address
import qualified Time
import qualified Transaction as Tx
import qualified SafeString
import qualified Storage

data Transaction = Transaction
  { header :: TransactionHeader
  , signature :: BS.ByteString
  , origin    :: Address.Address
  , timestamp :: Time.Timestamp
  } deriving (Show, Generic)

instance ToJSON Transaction where
  toJSON t =
    object
      [ "header"    .= header t
      , "signature" .= decodeUtf8 (signature t)
      , "timestamp" .= timestamp t
      , "origin"    .= origin t
      ]
instance FromJSON Transaction where
  parseJSON (Object v) = do
    hd   <- v .: "header"
    sig  <- v .: "signature"
    orig <- v .: "origin"
    t    <- v .: "timestamp"
    pure Transaction
      { header    = hd
      , signature = encodeUtf8 sig
      , origin    = orig
      , timestamp = t
      }

  parseJSON invalid = typeMismatch "Transaction" invalid

data TransactionHeader
  = TxContract TxContract
  | TxAsset Tx.TxAsset
  | TxAccount Tx.TxAccount
  deriving (Show, Generic)

instance S.Serialize TransactionHeader where
  put th =
    let parts =
          case th of
            TxContract tx -> S.encode tx
            TxAsset ax    -> S.encode ax
            TxAccount ax  -> S.encode ax
    in S.putByteString parts

instance ToJSON   TransactionHeader
instance FromJSON TransactionHeader

data TxContract
 = CreateContract {
     address :: Address.Address
   , script  :: SafeString.SafeString
   , ts      :: Time.Timestamp
   , owner   :: Address.Address
 }
 | Call {
     address :: Address.Address
   , method  :: SafeString.SafeString
    , args    :: [Storage.Value]
 }
 deriving (Show, Generic)

instance S.Serialize TxContract where
  put tx = case tx of
    CreateContract addr scr _ _ -> do
      S.putWord16be 1000
      Address.putAddress addr
      SafeString.putSafeString scr

    Call addr m a -> do
      let bs = SafeString.toBytes m
      S.putWord16be 1002
      Address.putAddress addr
      S.putWord64be (fromIntegral $ BS.length bs)
      S.putByteString (SafeString.toBytes m)
      S.put a

instance ToJSON   TxContract where
  toJSON (CreateContract addr scrpt ts' own) = object
    [ "tag"      .= ("CreateContract" :: Text)
    , "contents" .= object
        [ "address"   .= addr
        , "script"    .= scrpt
        , "timestamp" .= ts'
        , "owner"     .= own
        ]
    ]
  toJSON (Call addr m arg) = object
    [ "tag"       .= ("Call" :: Text)
    , "contents"  .= object
        [ "address" .= addr
        , "method"  .= m
        , "args"    .= arg
        ]
    ]
instance FromJSON TxContract

data InvalidTransaction = InvalidTransaction
  { transaction :: Transaction
  } deriving (Show, Generic)

instance ToJSON   InvalidTransaction
instance FromJSON InvalidTransaction
