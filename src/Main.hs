{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import System.Directory
import System.Environment   (getArgs)
import Prelude
import PlutusTx 
import Ledger               (TxOutRef(..))
import Ledger.Bytes         (LedgerBytes(LedgerBytes), fromHex)
import Plutus.V2.Ledger.Api    
import Cardano.Api.Shelley  (writeFileTextEnvelope) 
import qualified Data.ByteString as B
import Plutus.V2.Ledger.Api (TokenName)
import NFT                  (nftPlutusScript, nftCurrencySymbol, NFTParams(..))
import Token                (tokenPlutusScript)
import Validator            (validatorPlutusScript)
import Data.String          (IsString(fromString))
import                      Data.Text (Text, pack)

-- ***Construction of NFT parametres***
nftTokenName :: TokenName
nftTokenName = TokenName "Thread_NFT"

nftParams :: TxOutRef -> NFTParams 
nftParams txOutRef = NFTParams 
  { policyId = nftCurrencySymbol txOutRef nftTokenName
  , name = nftTokenName
  }

-- ***Input arguments for creating plutus scripts***
main :: IO ()
main = do
  args <- getArgs
  let argsLen = length args
  let Right refId = if argsLen > 0 then retrieveTxId (args !! 0) else error "Provide specific transaction hash with chosen output to spend it"
  let txOutRefIndex = if argsLen > 1 then read (args !! 1) :: Integer else error "Provide chosen output id in specific transaction"
  let txOutRef = TxOutRef refId txOutRefIndex
  let tokenName = if argsLen > 2 then args !! 2 else error "Provide token name"
  let filename = if argsLen > 3 then args !! 3 else tokenName
 
  putStrLn "Creating plutus scripts..."
  putStrLn $ "txOutRef => " ++ show txOutRef
  putStrLn $ "tokenName => " ++ show tokenName
  putStrLn $ "filename => " ++ show filename

  createPlutusScripts txOutRef (fromString tokenName) filename

retrieveTxId :: String -> Either Text TxId   
retrieveTxId s =         
    case fromHex (fromString s) of 
        Right (LedgerBytes bytes) -> Right $ TxId bytes 
        Left msg -> Left $ Data.Text.pack ("Could not convert from hex to bytes: " <> msg)

-- ***Record plutus scripts for specific token***
createPlutusScripts :: TxOutRef -> TokenName -> String -> IO ()
createPlutusScripts txOutRef tokenName filename = do
  exists <- doesDirectoryExist $ "./scripts/" ++ filename
  if exists
    then error "Filename is already in use for token"
    else createDirectory $ "./scripts/" ++ filename
  
  _ <- writeFileTextEnvelope ("./scripts/" ++ filename ++ "/nft.plutus") Nothing (nftPlutusScript txOutRef nftTokenName)
  _ <- writeFileTextEnvelope ("./scripts/" ++ filename ++ "/token.plutus") Nothing (tokenPlutusScript (nftParams txOutRef) tokenName)
  _ <- writeFileTextEnvelope ("./scripts/" ++ filename ++ "/validator.plutus") Nothing (validatorPlutusScript (nftParams txOutRef))
  return()