{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Token(
  tokenPolicySerializer,
  tokenPlutusScript,
  tokenUnappliedPlutusScript
) where

import           Cardano.Api.Shelley       (PlutusScript (PlutusScriptSerialised),
                                            PlutusScriptV2, serialiseToCBOR,
                                            writeFileTextEnvelope)
import qualified Codec.Serialise           as Serialise
import qualified Data.ByteString           as B
import           Data.ByteString.Base16    as B16
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import           NFT                       (NFTParams (..), hasNFT)
import           Plutus.Script.Utils.Typed as Scripts
import           Plutus.V1.Ledger.Value    (AssetClass (..), Value,
                                            assetClassValueOf, flattenValue)
import           Plutus.V2.Ledger.Api      (CurrencySymbol, Datum (Datum),
                                            MintingPolicy, OutputDatum (..),
                                            PubKeyHash (..),
                                            ScriptContext (scriptContextTxInfo),
                                            TokenName (..),
                                            TxInInfo (txInInfoResolved),
                                            TxInfo (txInfoMint),
                                            fromCompiledCode,
                                            mkMintingPolicyScript, txInfoInputs,
                                            txOutValue, unMintingPolicyScript)
import           Plutus.V2.Ledger.Contexts (ownCurrencySymbol, txOutDatum)
import           PlutusTx                  (CompiledCode, applyCode, compile,
                                            fromBuiltinData, liftCode,
                                            makeIsDataIndexed, makeLift)
import           PlutusTx.Prelude
import           PlutusTx.Prelude          (Bool (..), Integer, Maybe (..), all,
                                            elem, traceError, traceIfFalse, ($),
                                            (&&), (.), (==), (>))
import           Prelude                   (IO, Show (..), String, return)
import           ScriptUtils               (toPlutusScriptV2)

-- Custom data type to hold the owners' public keys
data ValidatorDatum = ValidatorDatum
    { owners       :: [PubKeyHash]
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
      info = scriptContextTxInfo ctx

      -- Check for minting token & whether the token is correct
      mintedValue :: Integer
      mintedValue = case flattenValue (txInfoMint info) of
        [(_, tn, amt)] ->
          if tn == tokenName
            then amt else traceError "incorrect token"
        _ -> traceError "expected one policy"

      -- Check type of transaction: minting or burning
      isMinting :: Bool
      isMinting = mintedValue > 0

      -- Check that Validator's output with NFT will be spent
      scriptValidationIsEnsured :: Bool
      scriptValidationIsEnsured =
        case [ o
                | i <- txInfoInputs info
                , let o = txInInfoResolved i
                , hasNFT nftparams $ txOutValue o
                ] of
          [_] -> True
          _   -> traceError "no NFT in input"
    in
      if isMinting
        then scriptValidationIsEnsured else True

policyUnapplied :: CompiledCode (NFTParams -> TokenName -> UntypedMintingPolicy)
policyUnapplied =
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap nparams tn = Scripts.mkUntypedMintingPolicy $ mkTokenPolicy nparams tn

policy :: NFTParams -> TokenName -> MintingPolicy
policy nftparams tokenName = mkMintingPolicyScript $
    policyUnapplied
    `PlutusTx.applyCode`
     PlutusTx.liftCode nftparams
     `PlutusTx.applyCode`
     PlutusTx.liftCode tokenName

-- Serialization
tokenPlutusScript :: NFTParams -> TokenName -> PlutusScript PlutusScriptV2
tokenPlutusScript nftparams tokenName = toPlutusScriptV2 $
        unMintingPolicyScript $ policy nftparams tokenName

-- Serialization
tokenUnappliedPlutusScript :: PlutusScript PlutusScriptV2
tokenUnappliedPlutusScript = toPlutusScriptV2 $
        fromCompiledCode $ policyUnapplied

tokenPolicySerializer :: NFTParams -> TokenName -> B.ByteString
tokenPolicySerializer nftparams tokenName = B16.encode $ serialiseToCBOR $ tokenPlutusScript nftparams tokenName
