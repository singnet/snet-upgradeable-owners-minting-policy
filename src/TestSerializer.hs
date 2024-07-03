{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module TestSerializer(
  testNftPolicySerializer,
  testTokenPolicySerializer,
  testValidatorScriptSerializer,
  createTestPlutusScripts,
  testNFTParams
) where

import           Cardano.Api.Shelley  (writeFileTextEnvelope)
import qualified Data.ByteString      as B
import           NFT                  (NFTParams (..), nftCurrencySymbol,
                                       nftPlutusScript, nftPolicySerializer,
                                       nftUnappliedPlutusScript)
import           Plutus.V2.Ledger.Api
import           Plutus.V2.Ledger.Api (TokenName)
import           PlutusTx
import           Prelude              (IO, Maybe (..), Show (..), String,
                                       return)
import           Token                (tokenPlutusScript, tokenPolicySerializer,
                                       tokenUnappliedPlutusScript)
import           Validator            (validatorPlutusScript,
                                       validatorScriptSerializer,
                                       validatorUnappliedPlutusScript)


--    FOR TESTING    --

-- ***NFT***

-- Only parameter to input manually, then `createTestPlutusScripts` can be executed
testOref :: TxOutRef
testOref = TxOutRef "f160ee01ff83321e37289837d6998273c4629667fb8f85b1a938fab81e01806b" 1

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
  _ <- writeFileTextEnvelope "./scripts/nft_unapplied.json" Nothing nftUnappliedPlutusScript
  _ <- writeFileTextEnvelope "./scripts/token_unapplied.json" Nothing tokenUnappliedPlutusScript
  _ <- writeFileTextEnvelope "./scripts/validator_unapplied.json" Nothing validatorUnappliedPlutusScript
  return()
