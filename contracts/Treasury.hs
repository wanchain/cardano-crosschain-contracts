{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns         #-}
-- {-# LANGUAGE FlexibleContexts   #-}
-- {-# LANGUAGE NamedFieldPuns     #-}
-- {-# LANGUAGE OverloadedStrings  #-}
-- {-# LANGUAGE TypeOperators      #-}
-- {-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
-- {-# OPTIONS_GHC -fno-specialise #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
-- {-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}

module CrossChain.Treasury
  ( treasuryScript
  -- , treasuryScriptShortBs
  ,treasuryScriptHash
  -- ,treasuryScriptHashStr
  ,treasuryAddress
  -- , TreasuryCheckRedeemer (..)
  -- ,TreasuryCheckRedeemer
  ,TreasuryParam (..)
  ,TreasuryParam
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($),(<>), (&&), (&&), (==),(||),(>=),(<),(-),not,length,filter,(>),(!!),map,head,reverse,any,elem,snd,mconcat,negate,divide)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS

-- import Plutus.Script.Utils.V2.Typed.Scripts.Validators as Scripts
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PV2
import Plutus.Script.Utils.V2.Scripts as Scripts
import Plutus.V2.Ledger.Api qualified as Plutus
import Plutus.V2.Ledger.Contexts as V2
import PlutusTx qualified
-- import PlutusTx.Builtins
import PlutusTx.Builtins
-- import PlutusTx.Eq as PlutusTx
-- import PlutusTx.Eq()
import PlutusTx.Prelude hiding (SemigroupInfo (..), unless, (.))
-- import PlutusTx.Prelude qualified as PlutusPrelude
import           Ledger               hiding (validatorHash,validatorHash)
import Plutus.V2.Ledger.Tx (isPayToScriptOut,OutputDatum (..))
import Ledger.Typed.Scripts (ValidatorTypes (..), TypedValidator (..),mkTypedValidator,mkTypedValidatorParam) --,mkUntypedValidator )
-- import Plutus.Script.Utils.Typed (validatorScript,validatorAddress,validatorHash)

import Data.ByteString qualified as ByteString
import Ledger.Crypto (PubKey (..), PubKeyHash, pubKeyHash)
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes),fromBytes,getLedgerBytes)
import Ledger.Ada  as Ada
import Plutus.V1.Ledger.Value (valueOf,currencySymbol,tokenName,symbols,flattenValue)
import PlutusTx.Builtins --(decodeUtf8,sha3_256,appendByteString)
import Ledger.Address 
import Ledger.Value
import Plutus.V2.Ledger.Contexts as V2
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import Plutus.V1.Ledger.Tx
-- ===================================================
-- import Plutus.V1.Ledger.Value
-- import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)

import Ledger hiding (validatorHash) --singleton
-- import Plutus.V1.Ledger.Scripts (getDatum)
import CrossChain.Types

data TreasuryParam
  = TreasuryParam
      { 
        groupInfoNFTCurrency :: CurrencySymbol
        , groupInfoNFTName :: TokenName
        -- groupInfoNFTCurrency :: CurrencySymbol
        -- , groupInfoNFTName :: TokenName
      } deriving stock (Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''TreasuryParam
PlutusTx.makeLift ''TreasuryParam

data GroupInfoParams
  = GroupInfoParams
      { params :: [BuiltinByteString]
      } deriving (Prelude.Eq, Prelude.Show)

PlutusTx.unstableMakeIsData ''GroupInfoParams

data ParamType = Version | Admin | GPK | BalanceWorker | TreasuryCheckVH | OracleWorker | MintCheckVH
PlutusTx.unstableMakeIsData ''ParamType

{-# INLINABLE getGroupInfoParams #-}
getGroupInfoParams :: GroupInfoParams -> ParamType -> BuiltinByteString
getGroupInfoParams (GroupInfoParams params) typeId = case typeId of
    Version -> params !! 0
    Admin -> params !! 1
    GPK -> params !! 2
    BalanceWorker -> params !! 3
    TreasuryCheckVH -> params !! 4
    OracleWorker -> params !! 5
    MintCheckVH -> params !! 6
    
data TreasuryType
instance Scripts.ValidatorTypes TreasuryType where
    type instance DatumType TreasuryType = ()
    type instance RedeemerType TreasuryType = ()

{-# INLINABLE groupInfoFromUtxo #-}
groupInfoFromUtxo :: V2.TxOut -> GroupInfoParams
groupInfoFromUtxo V2.TxOut{V2.txOutDatum=Plutus.OutputDatum datum} = case (Plutus.fromBuiltinData $ Plutus.getDatum datum) of
  Just groupInfo -> groupInfo

{-# INLINABLE mkValidator #-} -- V2.ScriptContext
mkValidator :: TreasuryParam -> () -> () -> V2.ScriptContext -> Bool
mkValidator (TreasuryParam groupInfoNFTCurrency groupInfoNFTName) _ _ ctx = 
  traceIfFalse "hat" hasTreasuryUtxoInput
  where
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    isGroupInfoToken :: V2.TxInInfo -> Bool
    isGroupInfoToken V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutValue}} = (assetClassValueOf txOutValue ( assetClass groupInfoNFTCurrency groupInfoNFTName)) > 0

    getGroupInfoTokenFromReferenceInputs :: V2.TxOut
    getGroupInfoTokenFromReferenceInputs = case filter (isGroupInfoToken) $ V2.txInfoReferenceInputs info of
        [o] -> V2.txInInfoResolved o
    
    getTreasuryCheck :: ValidatorHash
    getTreasuryCheck = 
      let !groupNFTUtxo = getGroupInfoTokenFromReferenceInputs
          !groupInfo = groupInfoFromUtxo groupNFTUtxo
          !treasuryCheck = ValidatorHash (getGroupInfoParams groupInfo TreasuryCheckVH)
          -- !treasuryCheckAddr = scriptHashAddress  treasuryCheck
      in treasuryCheck
    
    isInputOfTreasuryCheck :: V2.TxInInfo -> Bool
    isInputOfTreasuryCheck V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutAddress}} = 
      let !getTreasuryCheckVH = getTreasuryCheck
      in case txOutAddress of
            Address addressCredential _ -> case addressCredential of
              (Plutus.ScriptCredential s) -> s == getTreasuryCheckVH
              _ -> False
            _ -> False


    hasTreasuryUtxoInput :: Bool
    hasTreasuryUtxoInput = any (isInputOfTreasuryCheck) $ V2.txInfoInputs info


typedValidator :: TreasuryParam -> PV2.TypedValidator TreasuryType
typedValidator = PV2.mkTypedValidatorParam @TreasuryType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator


validator :: TreasuryParam -> Validator
validator = PV2.validatorScript . typedValidator

script :: TreasuryParam -> Plutus.Script
script = unValidatorScript . validator

-- treasuryScriptShortBs :: TreasuryParam -> SBS.ShortByteString
-- treasuryScriptShortBs = SBS.toShort . LBS.toStrict $ serialise . script

-- treasuryScript :: TreasuryParam -> PlutusScript PlutusScriptV2
-- treasuryScript = PlutusScriptSerialised . treasuryScriptShortBs

treasuryScript :: TreasuryParam ->  PlutusScript PlutusScriptV2
treasuryScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

treasuryScriptHash :: TreasuryParam -> Plutus.ValidatorHash
treasuryScriptHash = PV2.validatorHash .typedValidator

-- treasuryScriptHashStr :: TreasuryParam -> BuiltinByteString
-- treasuryScriptHashStr = case PlutusTx.fromBuiltinData $ PlutusTx.toBuiltinData . treasuryScriptHash of 
--   Just s -> s
--   Nothing -> ""

treasuryAddress ::TreasuryParam -> Ledger.Address
treasuryAddress = PV2.validatorAddress . typedValidator
