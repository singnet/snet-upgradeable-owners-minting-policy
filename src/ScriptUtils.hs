{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module ScriptUtils(
  toPlutusScriptV2,
  currencySymbol,
  plutusScriptToByteString,
  tracedUnsafeFrom
) where

import           Cardano.Api.Shelley            (PlutusScript (..),
                                                 PlutusScriptV2,
                                                 serialiseToCBOR,
                                                 writeFileTextEnvelope)
import           Codec.Serialise
import qualified Data.ByteString                as B
import qualified Data.ByteString.Base16         as B16
import qualified Data.ByteString.Lazy           as LBS
import qualified Data.ByteString.Short          as SBS
import           Plutus.Script.Utils.Typed      as Scripts
import qualified Plutus.Script.Utils.V2.Scripts as PSU.V2
import           Plutus.V1.Ledger.Value         (AssetClass (..), Value,
                                                 assetClassValueOf,
                                                 flattenValue, mpsSymbol)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Api           (CurrencySymbol, MintingPolicy,
                                                 TokenName, fromCompiledCode,
                                                 mkMintingPolicyScript)
import qualified Plutus.V2.Ledger.Api           as V2
import           Plutus.V2.Ledger.Contexts      as V2
import           PlutusTx
import           PlutusTx                       (CompiledCode, UnsafeFromData(unsafeFromBuiltinData))
import           Prelude                        (IO, Maybe (..), Show (..),
                                                 String, return, ($), (.))
import PlutusTx.Prelude (BuiltinData, BuiltinString, trace)

currencySymbol :: MintingPolicy -> CurrencySymbol
currencySymbol = mpsSymbol . PSU.V2.mintingPolicyHash

-- Serialization
toPlutusScriptV2 :: Script -> PlutusScript PlutusScriptV2
toPlutusScriptV2 script =
    PlutusScriptSerialised $
     SBS.toShort . LBS.toStrict $
      serialise $ script

plutusScriptToByteString :: PlutusScript PlutusScriptV2 -> B.ByteString
plutusScriptToByteString x = B16.encode $ serialiseToCBOR x

{-# INLINABLE tracedUnsafeFrom #-}
tracedUnsafeFrom :: forall a. UnsafeFromData a => BuiltinString -> BuiltinData -> a
tracedUnsafeFrom label d = trace label $ unsafeFromBuiltinData d