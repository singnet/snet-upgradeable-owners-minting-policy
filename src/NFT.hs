{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module NFT(
  hasNFT,
  nftPolicySerializer,
  testNftPolicySerializer,
  NFTParams(..), nftAsset, testNFTParams
) where

import Cardano.Api.Shelley                  (serialiseToCBOR, PlutusScript (..), PlutusScriptV2)
import Codec.Serialise
import Data.ByteString.Lazy qualified       as LBS
import Data.ByteString.Short qualified      as SBS
import Plutus.Script.Utils.Typed            as Scripts
import Plutus.V2.Ledger.Api qualified       as V2
import PlutusTx qualified
import PlutusTx.Prelude
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString            as B
import           Plutus.V1.Ledger.Value     (assetClassValueOf, Value, AssetClass(..), flattenValue)
import Plutus.V2.Ledger.Contexts            as V2
import           Plutus.V2.Ledger.Api       (CurrencySymbol, MintingPolicy, TokenName, mkMintingPolicyScript)
import           Prelude                    (Show (..))

-- data type for thread NFT 
data NFTParams = NFTParams 
    { policyId  :: CurrencySymbol
    , name      :: TokenName
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

policy :: TxOutRef -> TokenName -> MintingPolicy
policy outRef tokenName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode outRef
     `PlutusTx.applyCode`
     PlutusTx.liftCode tokenName
  where
    wrap out tn = Scripts.mkUntypedMintingPolicy $ mkNFTPolicy out tn

-- Serialization
nftPolicySerializer :: TxOutRef -> TokenName -> B.ByteString
nftPolicySerializer outRef tn =
  B16.encode $ serialiseToCBOR (
    PlutusScriptSerialised $
     SBS.toShort . LBS.toStrict $
      serialise $
        V2.unMintingPolicyScript $ policy outRef tn
    :: PlutusScript PlutusScriptV2)


-- For tests
testOref :: TxOutRef
testOref = TxOutRef "d36b6b3cf1dd77d703aac9fdc360485664064c1dac0189442d65707d99181e1d" 2

testNftPolicySerializer :: B.ByteString
testNftPolicySerializer = nftPolicySerializer testOref (name testNFTParams)


testNftPolicyId :: CurrencySymbol
testNftPolicyId = "443aaee673ee6282a71384cf90e9eb827f5deb415e303c7bdd85bafc"

testNFTParams :: NFTParams 
testNFTParams = NFTParams 
  {
    policyId = testNftPolicyId
  , name = V2.TokenName "Thread_NFT"
  }
