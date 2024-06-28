{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns          #-}

module Token(
  tokenPolicySerializer,
  tokenPlutusScript
) where

import Plutus.Script.Utils.Typed           as Scripts
import qualified Data.ByteString.Lazy      as LBS
import Plutus.V2.Ledger.Api                (TokenName(..), CurrencySymbol, MintingPolicy, 
                                            ScriptContext (scriptContextTxInfo), PubKeyHash(..), 
                                            txInfoInputs,
                                            TxInInfo (txInInfoResolved), TxInfo (txInfoMint), 
                                            OutputDatum(..), Datum(Datum), mkMintingPolicyScript,
                                            txOutValue, unMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (ownCurrencySymbol, txOutDatum)
import           Plutus.V1.Ledger.Value    (Value, flattenValue, AssetClass(..), assetClassValueOf)
import           PlutusTx                  (fromBuiltinData, makeIsDataIndexed, makeLift, applyCode, compile, liftCode)
import           PlutusTx.Prelude          (traceIfFalse, traceError, Bool(..), Integer, all, elem, ($), (.), (>), (&&), (==), Maybe(..))
import qualified Data.ByteString.Short     as SBS
import qualified Codec.Serialise           as Serialise
import           Data.ByteString.Base16    as B16
import           Cardano.Api.Shelley       (writeFileTextEnvelope, PlutusScript (PlutusScriptSerialised),
                                            PlutusScriptV2,serialiseToCBOR)   
import qualified Data.ByteString           as B
import           NFT                       (hasNFT, NFTParams(..))
import           Prelude                   (Show (..), String, IO, return)

-- Custom data type to hold the owners' public keys
data ValidatorDatum = ValidatorDatum
    { owners :: [PubKeyHash] 
    , minThreshold :: Integer
    }

PlutusTx.makeIsDataIndexed ''ValidatorDatum [('ValidatorDatum, 0)]
PlutusTx.makeLift ''ValidatorDatum


-- Token policy that mints tokens only if input with specific NFT is consumed 
{-# INLINABLE mkTokenPolicy #-}
mkTokenPolicy :: NFTParams -> TokenName -> () -> ScriptContext -> Bool
mkTokenPolicy nftparams tokenName _ ctx = 
    let
      info :: TxInfo
      !info = scriptContextTxInfo ctx

      -- Check for minting token & whether the token is correct
      mintedValue :: Integer
      !mintedValue = case flattenValue (txInfoMint info) of
        [(_, tn, amt)] ->
          if tn == tokenName
            then amt else traceError "incorrect token"
        _ -> traceError "expected one policy"

      -- Check type of transaction: minting or burning
      isMinting :: Bool
      !isMinting = mintedValue > 0
   
      -- Check that Validator's output with NFT will be spent 
      scriptValidationIsEnsured :: Bool
      scriptValidationIsEnsured = 
        case [ o 
                | i <- txInfoInputs info
                , let o = txInInfoResolved i
                , hasNFT nftparams $ txOutValue o 
                ] of
          [_] -> True
          _ -> traceError "no NFT in input"
    in
      if isMinting
        then scriptValidationIsEnsured else True

policy :: NFTParams -> TokenName -> MintingPolicy
policy nftparams tokenName = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode nftparams
     `PlutusTx.applyCode`
     PlutusTx.liftCode tokenName
  where
    wrap nparams tn = Scripts.mkUntypedMintingPolicy $ mkTokenPolicy nparams tn

-- Serialization
tokenPlutusScript :: NFTParams -> TokenName -> PlutusScript PlutusScriptV2
tokenPlutusScript nftparams tokenName =
    PlutusScriptSerialised $
     SBS.toShort . LBS.toStrict $
      Serialise.serialise $
        unMintingPolicyScript $ policy nftparams tokenName

tokenPolicySerializer :: NFTParams -> TokenName -> B.ByteString
tokenPolicySerializer nftparams tokenName = B16.encode $ serialiseToCBOR $ tokenPlutusScript nftparams tokenName
