{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module NFT(
  hasNFT,
  nftPolicySerializer,
  nftPlutusScript,
  nftCurrencySymbol,
  nftUnappliedPlutusScript,
  NFTParams(..), nftAsset
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
import           Plutus.V2.Ledger.Api           (CurrencySymbol, MintingPolicy,
                                                 TokenName, fromCompiledCode,
                                                 mkMintingPolicyScript)
import qualified Plutus.V2.Ledger.Api           as V2
import           Plutus.V2.Ledger.Contexts      as V2
import           PlutusTx                       (CompiledCode)
import qualified PlutusTx
import           PlutusTx.Prelude
import           Prelude                        (IO, Show (..), String)
import           ScriptUtils                    (toPlutusScriptV2)

-- data type for thread NFT
data NFTParams = NFTParams
    { policyId :: CurrencySymbol
    , name     :: TokenName
    } deriving Show

PlutusTx.makeIsDataIndexed ''NFTParams [('NFTParams, 0)]
PlutusTx.makeLift ''NFTParams

{-# INLINEABLE nftAsset #-}
nftAsset :: NFTParams -> AssetClass
nftAsset nftparams = AssetClass (policyId nftparams, name nftparams)

-- Helper function to check if the value contains the correct NFT
{-# INLINABLE hasNFT #-}
hasNFT :: NFTParams -> Value -> Bool
hasNFT nftparams value = assetClassValueOf value (nftAsset nftparams) == 1

-- NFT minting policy
{-# INLINABLE mkNFTPolicy #-}
mkNFTPolicy :: TxOutRef -> TokenName -> () -> V2.ScriptContext -> Bool
mkNFTPolicy oref tn _ ctx = traceIfFalse "UTxO not consumed"   hasUTxO &&
                            traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    hasUTxO :: Bool
    hasUTxO = any (\i -> txInInfoOutRef i == oref) $ txInfoInputs info

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint info) of
        [(_, tn'', amt)] -> tn'' == tn && amt == 1
        _                -> False

policyUnapplied :: CompiledCode (TxOutRef -> TokenName -> BuiltinData -> BuiltinData -> ())
policyUnapplied =
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap out tn = Scripts.mkUntypedMintingPolicy $ mkNFTPolicy out tn

policy :: TxOutRef -> TokenName -> MintingPolicy
policy outRef tokenName = mkMintingPolicyScript $
    policyUnapplied
    `PlutusTx.applyCode`
     PlutusTx.liftCode outRef
     `PlutusTx.applyCode`
     PlutusTx.liftCode tokenName

currencySymbol :: MintingPolicy -> CurrencySymbol
currencySymbol = mpsSymbol . PSU.V2.mintingPolicyHash

nftCurrencySymbol :: TxOutRef -> TokenName -> CurrencySymbol
nftCurrencySymbol outRef tokenName = currencySymbol (policy outRef tokenName)

-- Serialization
nftPlutusScript :: TxOutRef -> TokenName -> PlutusScript PlutusScriptV2
nftPlutusScript outRef tn = toPlutusScriptV2 $
  V2.unMintingPolicyScript $ policy outRef tn

-- Serialization
nftUnappliedPlutusScript :: PlutusScript PlutusScriptV2
nftUnappliedPlutusScript = toPlutusScriptV2 $ fromCompiledCode policyUnapplied

nftPolicySerializer :: TxOutRef -> TokenName -> B.ByteString
nftPolicySerializer outRef tn = B16.encode $ serialiseToCBOR $ nftPlutusScript outRef tn
