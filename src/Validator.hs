{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module Validator(
  validatorScriptSerializer,
  validatorPlutusScript,
  validatorUnappliedPlutusScript
) where

import           Cardano.Api.Shelley       (PlutusScript (PlutusScriptSerialised),
                                            PlutusScriptV2, displayError,
                                            serialiseToCBOR,
                                            writeFileTextEnvelope)
import           Codec.Serialise           as Serialise
import           Data.ByteString           as B
import           Data.ByteString.Base16    as B16
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.ByteString.Short     as SBS
import           NFT                       (NFTParams (..), hasNFT)
import           Plutus.Script.Utils.Typed as Scripts
import           Plutus.V1.Ledger.Value    as Value
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Api      (CurrencySymbol, MintingPolicy,
                                            TokenName, fromCompiledCode,
                                            mkMintingPolicyScript)
import qualified Plutus.V2.Ledger.Api      as V2
import           Plutus.V2.Ledger.Contexts as PV2
import           PlutusTx
import           PlutusTx.Prelude          as PP
import           Prelude                   (Eq, IO, Show, String, show)
import           ScriptUtils               (toPlutusScriptV2)

-- Datum type
data MultiSigDatum = MultiSigDatum
    { owners  :: [PubKeyHash]
    , minSigs :: Integer
    } deriving (Show)

PlutusTx.makeIsDataIndexed ''MultiSigDatum [('MultiSigDatum, 0)]
PlutusTx.makeLift ''MultiSigDatum

-- Redeemer type
data MultisigRedeemer =
      TokenMint
    | AddOwner PubKeyHash Integer
    | RemoveOwner PubKeyHash Integer
    | UpdateThreshold Integer
  deriving (Show)

PlutusTx.makeIsDataIndexed ''MultisigRedeemer [ ('TokenMint, 0)
                                              , ('AddOwner, 1)
                                              , ('RemoveOwner, 2)
                                              , ('UpdateThreshold, 3)
                                              ]
PlutusTx.makeLift ''MultisigRedeemer


{-# INLINABLE upgradeableOwnersValidator #-}
upgradeableOwnersValidator :: NFTParams -> MultiSigDatum -> MultisigRedeemer -> ScriptContext -> Bool
upgradeableOwnersValidator nftparams datum redeemer ctx =
    traceIfFalse "Not enough owners signed tx" checkMinSigs &&
    case redeemer of
      TokenMint                         -> traceIfFalse "Datum was changed"
                                            (checkConsistencyOfOutputDatum (owners datum) (minSigs datum))
      AddOwner newOwner newThreshold    -> traceIfFalse "Owner not added"
                                            (checkConsistencyOfOutputDatum (newOwner : owners datum) newThreshold) &&
                                            (checkThresholdInterval newThreshold (PP.length (owners datum) PP.+ 1))
      RemoveOwner oldOwner newThreshold -> traceIfFalse "Owner not removed"
                                            (checkConsistencyOfOutputDatum (PP.filter (/= oldOwner) (owners datum)) newThreshold) &&
                                            (checkThresholdInterval newThreshold (PP.length (owners datum) PP.- 1))
      UpdateThreshold newThreshold      -> traceIfFalse "Threshold not updated"
                                            (checkConsistencyOfOutputDatum (owners datum) newThreshold) &&
                                            (checkThresholdInterval newThreshold (PP.length (owners datum)))

  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMinSigs :: Bool
    checkMinSigs = PP.length (PP.filter (`PP.elem` txInfoSignatories info) (owners datum)) >= minSigs datum

    getOutputWithNFT :: TxOut
    getOutputWithNFT = case PV2.getContinuingOutputs ctx of
        [out] | hasNFT nftparams $ txOutValue $ out -> out
        _ -> traceError "no output with nft"

    getContinuingOutputDatum :: MultiSigDatum
    getContinuingOutputDatum = case txOutDatum getOutputWithNFT of
        OutputDatum d -> case fromBuiltinData (getDatum d) of
          Nothing            -> traceError "invalid datum type"
          Just multiSigDatum -> multiSigDatum
        _  -> traceError "no inline datum"

    checkConsistencyOfOutputDatum :: [PubKeyHash] -> Integer -> Bool
    checkConsistencyOfOutputDatum expectedOwners expectedThreshold =
      let
        outputDatum = getContinuingOutputDatum
      in
        if expectedOwners == (owners outputDatum) && expectedThreshold == (minSigs outputDatum)
          then True else traceError "unexpected output datum"

    checkThresholdInterval :: Integer -> Integer -> Bool
    checkThresholdInterval expectedThreshold maxThreshold = expectedThreshold >= 2 && expectedThreshold <= maxThreshold

validatorUnapplied :: CompiledCode (NFTParams -> UntypedValidator)
validatorUnapplied =
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap nparams = Scripts.mkUntypedValidator $ upgradeableOwnersValidator nparams

validator :: NFTParams -> V2.Validator
validator nftparams = V2.mkValidatorScript $
    validatorUnapplied
    `PlutusTx.applyCode`
     PlutusTx.liftCode nftparams

-- Serialization
validatorPlutusScript :: NFTParams -> PlutusScript PlutusScriptV2
validatorPlutusScript nftparams =
    PlutusScriptSerialised $
     SBS.toShort . LBS.toStrict $
      Serialise.serialise $
        validator nftparams

validatorUnappliedPlutusScript :: PlutusScript PlutusScriptV2
validatorUnappliedPlutusScript = toPlutusScriptV2 $ fromCompiledCode validatorUnapplied

validatorScriptSerializer :: NFTParams -> B.ByteString
validatorScriptSerializer nftparams = B16.encode $ serialiseToCBOR $ validatorPlutusScript nftparams

