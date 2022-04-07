{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
-- Copyright: © 2018-2022 IOHK
-- License: Apache-2.0
--
-- A wrapper around TxMetadata to allow different JSON codecs. (ADP-1596)
module Cardano.Wallet.Api.Types.SchemaMetadata where

import Cardano.Api
  ( Error (displayError)
  , TxMetadataJsonSchema (TxMetadataJsonDetailedSchema, TxMetadataJsonNoSchema)
  , metadataFromJson
  , metadataToJson
  )
import Cardano.Wallet.Primitive.Types.Tx (TxMetadata (TxMetadata))
import Control.DeepSeq (NFData)
import Data.Aeson ( ToJSON(toJSON), FromJSON(parseJSON) )
import Data.Generics.Internal.VL (Iso', view)
import Data.Generics.Wrapped (_Unwrapped)
import GHC.Generics (Generic)
import Prelude

data MetadataSchema = MetadataNoSchema | MetadataDetailedSchema

newtype SchemaTxMetadata (schema :: MetadataSchema) = SchemaTxMetadata TxMetadata
  deriving (Show, Eq, Generic)
  deriving anyclass (NFData)

schemaTxMetadataIso :: Iso' (SchemaTxMetadata schema) TxMetadata
schemaTxMetadataIso = _Unwrapped

instance ToJSON (SchemaTxMetadata 'MetadataDetailedSchema) where
  toJSON = metadataToJson TxMetadataJsonDetailedSchema . view schemaTxMetadataIso

instance FromJSON (SchemaTxMetadata 'MetadataDetailedSchema) where
  parseJSON =
    fmap SchemaTxMetadata
      . either (fail . displayError) pure
      . metadataFromJson TxMetadataJsonDetailedSchema

instance ToJSON (SchemaTxMetadata 'MetadataNoSchema) where
  toJSON = metadataToJson TxMetadataJsonNoSchema . view schemaTxMetadataIso

instance FromJSON (SchemaTxMetadata 'MetadataNoSchema) where
  parseJSON =
    fmap SchemaTxMetadata
      . either (fail . displayError) pure
      . metadataFromJson TxMetadataJsonNoSchema