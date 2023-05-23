
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns       #-}

module CrossChain.MappingToken
  ( mappingTokenScript
  , mappingTokenScriptShortBs
  , mappingTokenCurSymbol
  -- , CheckTokenInfo (..)
  ) where


import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&), (&&), (==),(-),(!!),(||),(<),not,length,filter,(>),map,head,any)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as V2
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol)
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (SemigroupInfo (..), unless, (.))

import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)
import Plutus.V1.Ledger.Value
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
import Ledger.Crypto (PubKey (..), PubKeyHash)
import Data.Aeson (FromJSON, ToJSON)
import PlutusTx (BuiltinData, CompiledCode, Lift, applyCode, liftCode, fromData)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Typed (validatorScript,validatorAddress,validatorHash)
-- import Plutus.V1.Ledger.Scripts (unValidatorScript)
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import CrossChain.Types 



{-# INLINABLE groupInfoFromUtxo #-}
groupInfoFromUtxo :: V2.TxOut -> GroupInfoParams
groupInfoFromUtxo V2.TxOut{V2.txOutDatum=V2.OutputDatum datum} = case (V2.fromBuiltinData $ V2.getDatum datum) of
  Just groupInfo -> groupInfo

{-# INLINABLE mkPolicy #-}
mkPolicy :: CheckTokenInfo -> () -> ScriptContext -> Bool
mkPolicy  (CheckTokenInfo checkTokenSymbol checkTokenName) _ ctx =
  if isBurn 
    then True 
  else 
    traceIfFalse "hmm" hasCheckTokenInput
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    -- isGroupInfoToken :: V2.TxInInfo -> Bool
    -- isGroupInfoToken V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutValue}} = (assetClassValueOf txOutValue ( assetClass groupInfoNFTCurrency groupInfoNFTName)) > 0
    
    -- getGroupInfoTokenFromReferenceInputs :: V2.TxOut
    -- getGroupInfoTokenFromReferenceInputs = case filter (isGroupInfoToken) $ V2.txInfoReferenceInputs info of
    --     [o] -> V2.txInInfoResolved o
    
    -- getMintCheck :: V2.ValidatorHash
    -- getMintCheck = 
    --   let !groupNFTUtxo = getGroupInfoTokenFromReferenceInputs
    --       !groupInfo = groupInfoFromUtxo groupNFTUtxo
    --       !treasuryCheck = V2.ValidatorHash (getGroupInfoParams groupInfo MintCheckVH)
    --       -- !treasuryCheckAddr = scriptHashAddress  treasuryCheck
    --   in treasuryCheck
    
    -- isInputOfTreasuryCheck :: V2.TxInInfo -> Bool
    -- isInputOfTreasuryCheck V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutAddress}} = 
    --   let !getMintCheckVH = getMintCheck
    --   in case txOutAddress of
    --         V2.Address addressCredential _ -> case addressCredential of
    --           (V2.ScriptCredential s) -> s == getMintCheckVH
    --           _ -> False
    --         _ -> False


    -- hasCheckUtxoInput :: Bool
    -- hasCheckUtxoInput = any (isInputOfTreasuryCheck) $ V2.txInfoInputs info

    hasCheckTokenInput :: Bool
    !hasCheckTokenInput = 
      let !totalInputValue = V2.valueSpent info
          !amount = valueOf totalInputValue checkTokenSymbol checkTokenName
      in amount == 1

    isBurn :: Bool
    isBurn = case flattenValue $ V2.txInfoMint info of
        [(symbol,_,a)] -> (symbol == ownCurrencySymbol ctx) && (a < 0)

policy :: CheckTokenInfo -> V2.MintingPolicy
policy oref = V2.mkMintingPolicyScript $ $$(PlutusTx.compile [|| \c -> V2.mkUntypedMintingPolicy (mkPolicy c)  ||]) 
    `PlutusTx.applyCode` PlutusTx.liftCode oref


mappingTokenCurSymbol :: CheckTokenInfo -> CurrencySymbol
mappingTokenCurSymbol = scriptCurrencySymbol . policy

plutusScript :: CheckTokenInfo -> V2.Script
plutusScript = V2.unMintingPolicyScript . policy

validator :: CheckTokenInfo -> V2.Validator
validator = V2.Validator . plutusScript

scriptAsCbor :: CheckTokenInfo -> LBS.ByteString
scriptAsCbor = serialise . validator

mappingTokenScript :: CheckTokenInfo -> PlutusScript PlutusScriptV2
mappingTokenScript mgrData = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict (scriptAsCbor mgrData)

mappingTokenScriptShortBs :: CheckTokenInfo -> SBS.ShortByteString
mappingTokenScriptShortBs mgrData = SBS.toShort . LBS.toStrict $ (scriptAsCbor mgrData)