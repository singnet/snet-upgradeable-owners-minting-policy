{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module TestSerializer(
  testNftPolicySerializer,
  testTokenPolicySerializer,
  testValidatorScriptSerializer,
  createTestPlutusScripts,
  testNFTParams
) where

import Prelude             (Show (..), String, IO, Maybe(..), return)
import PlutusTx 
import Plutus.V2.Ledger.Api    
import Cardano.Api.Shelley  (writeFileTextEnvelope) 
import qualified Data.ByteString as B
import Plutus.V2.Ledger.Api (TokenName)
import NFT                  (nftPolicySerializer, nftPlutusScript, nftCurrencySymbol, NFTParams(..))
import Token                (tokenPolicySerializer, tokenPlutusScript)
import Validator            (validatorScriptSerializer, validatorPlutusScript)


--    FOR TESTING    --

-- ***NFT***

-- Only parameter to input manually, then `createTestPlutusScripts` can be executed
testOref :: TxOutRef
testOref = TxOutRef "8ea78813126065dc1e927b84f9705aea62961bfa12e57c9ce0a53cab8d7993ec" 2

testNFTTokenName :: TokenName
testNFTTokenName = TokenName "Thread_NFT"

testNftPolicySerializer :: B.ByteString
testNftPolicySerializer = nftPolicySerializer testOref testNFTTokenName

testNFTParams :: NFTParams 
testNFTParams = NFTParams 
  {
    policyId = nftCurrencySymbol testOref testNFTTokenName
  , name = testNFTTokenName
  }

-- ***Token***
testTokenName :: TokenName
testTokenName = TokenName "TToken"

testTokenPolicySerializer :: B.ByteString
testTokenPolicySerializer = tokenPolicySerializer testNFTParams testTokenName

-- ***Validator: Upgradeable owners***
testValidatorScriptSerializer :: B.ByteString
testValidatorScriptSerializer = validatorScriptSerializer testNFTParams


createTestPlutusScripts :: IO ()
createTestPlutusScripts = do
  _ <- writeFileTextEnvelope "./scripts/nft.json" Nothing (nftPlutusScript testOref testNFTTokenName)
  _ <- writeFileTextEnvelope "./scripts/token.json" Nothing (tokenPlutusScript testNFTParams testTokenName)
  _ <- writeFileTextEnvelope "./scripts/validator.json" Nothing (validatorPlutusScript testNFTParams)
  return()