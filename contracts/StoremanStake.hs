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


module CrossChain.StoremanStake
  ( storemanStakeScript
  , storemanStakeScriptShortBs
  , storemanStakeHash
--   , storemanStakeAddress
--   , StoremanStakeProof (..)
--   , StoremanStakeRedeemer (..)
  , GroupNFTTokenInfo (..)
  , GroupNFTTokenInfo
  ) where


import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&), (&&), (==),(-),(!!),(||),(<),not,length,filter,(>),map,head,any)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

import Plutus.Script.Utils.V2.Typed.Scripts.MonetaryPolicies as V2
import Plutus.Script.Utils.V2.Typed.Scripts.StakeValidators (mkUntypedStakeValidator)
import Plutus.Script.Utils.V2.Scripts (scriptCurrencySymbol, stakeValidatorHash)
import Plutus.V2.Ledger.Api qualified as V2
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (SemigroupInfo (..), unless, (.))

import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash,Address (..))
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


-- data StoremanStakeRedeemer = StoremanStakeRedeemer
--   {
--     toPkhPay :: BuiltinByteString -- send to 
--     , toPkhStk :: BuiltinByteString
--     , policy :: BuiltinByteString -- which token , zero indicated only transfer ada
--     , assetName :: BuiltinByteString
--     , amount :: Integer  -- token amount
--     -- , adaAmount :: Integer -- addtional ada amount
--     , txHash :: BuiltinByteString
--     , index :: Integer
--     , mode :: Integer
--     , uniqueId :: BuiltinByteString
--     -- , txType :: Integer
--     , ttl :: Integer
--     , signature :: BuiltinByteString
--   }
--     deriving (Show, Prelude.Eq)
-- PlutusTx.unstableMakeIsData ''StoremanStakeRedeemer


{-# INLINABLE mkStakeValidator #-}
mkStakeValidator :: GroupNFTTokenInfo -> () -> V2.ScriptContext -> Bool
mkStakeValidator (GroupNFTTokenInfo groupNftSymbol groupNftName) _ ctx = 
    traceIfFalse "hats" hasStackeCheckInput
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    isGroupInfoToken :: V2.TxOut -> Bool
    isGroupInfoToken (V2.TxOut (Address credential _) txOutValue _ _) = (assetClassValueOf txOutValue ( assetClass groupNftSymbol groupNftName)) > 0

    refUtxosOfGroupInfo :: [V2.TxOut]
    refUtxosOfGroupInfo = map (V2.txInInfoResolved) $ txInfoReferenceInputs info

    getGroupInfoTokenFromReferenceInputs :: V2.TxOut
    getGroupInfoTokenFromReferenceInputs = case filter (isGroupInfoToken) refUtxosOfGroupInfo of
        [o] -> o
        -- _ -> traceError "o"-- "expected exactly one groupInfotoken reference inputs"

    groupInfo :: GroupInfoParams
    groupInfo = 
      case getGroupInfoTokenFromReferenceInputs of
        (V2.TxOut _ _ outputDatum _) -> case outputDatum of
          (V2.OutputDatum datum ) -> case (V2.fromBuiltinData $ V2.getDatum datum) of
            Just groupInfo -> groupInfo
    

    isInputOfStackCheck :: V2.TxInInfo -> Bool
    isInputOfStackCheck V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutAddress}} = 
      let stakeCheckVH = V2.ValidatorHash (getGroupInfoParams groupInfo StakeCheckVH)
      in case txOutAddress of
            Address addressCredential _ -> case addressCredential of
              (V2.ScriptCredential s) -> s == stakeCheckVH
              _ -> False
            _ -> False


    hasStackeCheckInput :: Bool
    hasStackeCheckInput = any (isInputOfStackCheck) $ V2.txInfoInputs info



stakeValidator :: GroupNFTTokenInfo -> V2.StakeValidator
stakeValidator param =
    V2.mkStakeValidatorScript $ $$(PlutusTx.compile [|| \c -> mkUntypedStakeValidator (mkStakeValidator c) ||])
      `PlutusTx.applyCode` PlutusTx.liftCode param

storemanStakeHash :: GroupNFTTokenInfo -> V2.StakeValidatorHash
storemanStakeHash = stakeValidatorHash . stakeValidator

plutusScript :: GroupNFTTokenInfo -> V2.Script
plutusScript = V2.unStakeValidatorScript . stakeValidator

validator :: GroupNFTTokenInfo -> V2.Validator
validator = V2.Validator . plutusScript

scriptAsCbor :: GroupNFTTokenInfo -> LBS.ByteString
scriptAsCbor = serialise . validator

storemanStakeScript :: GroupNFTTokenInfo -> PlutusScript PlutusScriptV2
storemanStakeScript param = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict (scriptAsCbor param)

storemanStakeScriptShortBs :: GroupNFTTokenInfo -> SBS.ShortByteString
storemanStakeScriptShortBs param = SBS.toShort . LBS.toStrict $ (scriptAsCbor param)

-- storemanStakeAddress :: GroupNFTTokenInfo -> Address
-- storemanStakeAddress
