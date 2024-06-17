{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns          #-}

module Validator(
    validatorScriptSerializer,
    testValidatorScriptSerializer
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
import Prelude                                 (Show)
import Cardano.Api.Shelley                     (PlutusScript (PlutusScriptSerialised),
                                                PlutusScriptV2, serialiseToCBOR)   
import NFT                                     (hasNFT, NFTParams(..), testNFTParams)


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
    | AddAddress PubKeyHash 
    | RemoveAddress PubKeyHash 
    | UpdateThreshold Integer
  deriving (Show)
                  
PlutusTx.makeIsDataIndexed ''MultisigRedeemer [ ('TokenMint, 0)
                                              , ('AddAddress, 1)
                                              , ('RemoveAddress, 2)
                                              , ('UpdateThreshold, 3)
                                              ]
PlutusTx.makeLift ''MultisigRedeemer


{-# INLINABLE upgradeableAddressesValidator #-}
upgradeableAddressesValidator :: NFTParams -> MultiSigDatum -> MultisigRedeemer -> ScriptContext -> Bool
upgradeableAddressesValidator nftparams datum redeemer ctx = 
    traceIfFalse "Not enough owners signed tx" checkMinSigs &&
    case redeemer of
        TokenMint                    -> (owners datum == getUpdatedOwners (minSigs datum)) 
        AddAddress newAddr           -> traceIfFalse "Address not added"
                                          ((newAddr : owners datum) == getUpdatedOwners (minSigs datum))
        RemoveAddress oldAddr        -> traceIfFalse "Address not removed" 
                                          ((PP.filter (/= oldAddr) (owners datum)) == 
                                            getUpdatedOwners (minSigs datum))
        UpdateThreshold newThreshold -> traceIfFalse "Threshold not updated" 
                                          (owners datum == getUpdatedOwners newThreshold &&
                                           newThreshold >= 2)
  where
    info :: TxInfo
    !info = scriptContextTxInfo ctx

    getOutputWithNFT :: TxOut
    !getOutputWithNFT = case PV2.getContinuingOutputs ctx of
        [out] | hasNFT nftparams $ txOutValue $ out -> out
        _ -> traceError "no output with nft"

    -- returns owners from output with NFT
    getUpdatedOwners :: Integer -> [PubKeyHash]
    getUpdatedOwners expectedThreshold = case txOutDatum getOutputWithNFT of
        OutputDatum d -> case fromBuiltinData (getDatum d) of
            Nothing                               -> traceError "invalid datum"
            Just (MultiSigDatum owners' minSigs') -> 
                if minSigs' == expectedThreshold then owners' else traceError "unexpected threshold update"
        _             -> traceError "no inline datum"
      
    checkMinSigs :: Bool
    checkMinSigs = PP.length (PP.filter (`PP.elem` txInfoSignatories info) (owners datum)) >= minSigs datum


validator :: NFTParams -> V2.Validator
validator nftparams = V2.mkValidatorScript $
    $$(PlutusTx.compile [|| wrap ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode nftparams
  where
    wrap nparams = Scripts.mkUntypedValidator $ upgradeableAddressesValidator nparams

validatorScriptSerializer :: NFTParams -> B.ByteString
validatorScriptSerializer nftparams =
  B16.encode $ serialiseToCBOR (
    PlutusScriptSerialised $
     SBS.toShort . LBS.toStrict $
      Serialise.serialise $
        validator nftparams
    :: PlutusScript PlutusScriptV2)


-- For tests
testValidatorScriptSerializer :: B.ByteString
testValidatorScriptSerializer = validatorScriptSerializer testNFTParams
