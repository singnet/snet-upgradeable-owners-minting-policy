{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DeriveAnyClass        #-}

module Validator(
  validatorScriptSerializer,
  validatorPlutusScript
) where

import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts               as PV2
import PlutusTx
import PlutusTx.Prelude                        as PP
import Plutus.V1.Ledger.Value                  as Value
import Plutus.V2.Ledger.Api          qualified as V2
import Plutus.Script.Utils.Typed               as Scripts
import Data.ByteString.Lazy          qualified as LBS
import Data.ByteString.Short         qualified as SBS
import Data.ByteString.Base16                  as B16
import Data.ByteString                         as B
import Codec.Serialise                         as Serialise
import Prelude                                 (Eq, Show, IO, show, String)
import Cardano.Api.Shelley                     (displayError, writeFileTextEnvelope, PlutusScript (PlutusScriptSerialised),
                                                PlutusScriptV2, serialiseToCBOR)   
import NFT                                     (hasNFT, NFTParams(..))


-- Datum type
data MultiSigDatum = MultiSigDatum
    { owners    :: [PubKeyHash]
    , minSigs   :: Integer
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

validator :: NFTParams -> V2.Validator
validator nftparams = V2.mkValidatorScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode nftparams
  where
    wrap nparams = Scripts.mkUntypedValidator $ upgradeableOwnersValidator nparams


-- Serialization
validatorPlutusScript :: NFTParams -> PlutusScript PlutusScriptV2
validatorPlutusScript nftparams =
    PlutusScriptSerialised $
     SBS.toShort . LBS.toStrict $
      Serialise.serialise $
        validator nftparams

validatorScriptSerializer :: NFTParams -> B.ByteString
validatorScriptSerializer nftparams = B16.encode $ serialiseToCBOR $ validatorPlutusScript nftparams

