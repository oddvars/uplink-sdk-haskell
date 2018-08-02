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

import qualified Address as A
import qualified Time
import qualified Transaction as Tx
import qualified SafeString
import qualified Storage

data Transaction = Transaction
  { header :: TransactionHeader
  , signature :: BS.ByteString
  , origin    :: A.Address A.AAccount
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
  put th = do
    let parts =
          case th of
            TxContract tx -> (0, S.encode tx)
            TxAsset ax    -> (1, S.encode ax)
            TxAccount ax  -> (2, S.encode ax)
    S.putWord16be   (fst parts)
    S.putByteString (snd parts)

instance ToJSON   TransactionHeader
instance FromJSON TransactionHeader

data TxContract
 = CreateContract {
     contract :: SafeString.SafeString
 }
 | Call {
     address :: A.Address A.AAccount
   , method  :: SafeString.SafeString
    , args    :: [Storage.Value]
 }
 deriving (Show, Generic)

instance S.Serialize TxContract where
  put tx = case tx of
    CreateContract con -> do
      S.putWord16be 0
      SafeString.putSafeString con

    Call addr m a -> do
      let bs = SafeString.toBytes m
      S.putWord16be 2
      A.putAddress addr
      S.putWord64be (fromIntegral $ BS.length bs)
      S.putByteString (SafeString.toBytes m)
      S.put a

instance ToJSON   TxContract where
  toJSON (CreateContract con) = object
    [ "tag"      .= ("CreateContract" :: Text)
    , "contents" .= object [ "contract"   .= con ]
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
