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
  -- , TreasuryCheckProof (..)
  -- ,TreasuryCheckProof
  ,CheckTokenInfoParam (..)
  ,CheckTokenInfoParam
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($),(<>), (&&), (||),(>=),(<),(==),(-),not,length,filter,foldMap,(>),(!!),map,head,reverse,any,elem,snd,mconcat,negate,divide)

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
-- import PlutusTx.Prelude (Eq)
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
import Plutus.Script.Utils.V2.Address (mkValidatorAddress)
-- ===================================================
-- import Plutus.V1.Ledger.Value
-- import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)

import Ledger hiding (validatorHash) --singleton
-- import Plutus.V1.Ledger.Scripts (getDatum)
import CrossChain.Types 


-- data CheckTokenInfoParam
--   = CheckTokenInfoParam
--       { 
--         -- groupInfoNFTCurrency :: CurrencySymbol
--         -- , groupInfoNFTName :: TokenName
--         checkTokenSymbol :: CurrencySymbol
--         , checkTokenName :: TokenName
--       } deriving stock (Generic)
--         deriving anyclass (ToJSON, FromJSON)

-- PlutusTx.unstableMakeIsData ''CheckTokenInfoParam
-- PlutusTx.makeLift ''CheckTokenInfoParam

-- data GroupInfoParams
--   = GroupInfoParams
--       { params :: [BuiltinByteString]
--       } deriving (Prelude.Eq, Prelude.Show)

-- PlutusTx.unstableMakeIsData ''GroupInfoParams

-- data ParamType = Version | Admin | GPK | BalanceWorker | TreasuryCheckVH | OracleWorker | MintCheckVH | StkPKh
-- PlutusTx.unstableMakeIsData ''ParamType

-- {-# INLINABLE getGroupInfoParams #-}
-- getGroupInfoParams :: GroupInfoParams -> ParamType -> BuiltinByteString
-- getGroupInfoParams (GroupInfoParams params) typeId = case typeId of
--     Version -> params !! 0
--     Admin -> params !! 1
--     GPK -> params !! 2
--     BalanceWorker -> params !! 3
--     TreasuryCheckVH -> params !! 4
--     OracleWorker -> params !! 5
--     MintCheckVH -> params !! 6
--     StkPKh -> params !! 7
    
-- data TreasuryType
-- instance Scripts.ValidatorTypes TreasuryType where
--     type instance DatumType TreasuryType = ()
--     type instance RedeemerType TreasuryType = ()

-- {-# INLINABLE groupInfoFromUtxo #-}
-- groupInfoFromUtxo :: TxOut' -> GroupInfoParams
-- groupInfoFromUtxo TxOut'{txOutDatum'=Plutus.OutputDatum datum} = case (Plutus.fromBuiltinData $ Plutus.getDatum datum) of
--   Just groupInfo -> groupInfo


-- findOwnInput' :: V2.ScriptContext -> Maybe V2.TxInInfo
-- findOwnInput' V2.ScriptContext{V2.scriptContextTxInfo=V2.TxInfo{V2.txInfoInputs},
--              V2.scriptContextPurpose=Spending txOutRef} = go txInfoInputs
--     where
--         go [] = Nothing
--         go (i@V2.TxInInfo{V2.txInInfoOutRef} : rest) = if txInInfoOutRef == txOutRef
--                                                  then Just i
--                                                  else go rest
-- findOwnInput' _ = Nothing

{-# INLINABLE mkValidator #-} -- V2.ScriptContext
mkValidator :: CheckTokenInfoParam -> () -> () -> BuiltinData -> Bool
mkValidator (CheckTokenInfoParam checkTokenSymbol checkTokenName) _ _ rawContext = -- True
  traceIfFalse "hat" hasTreasuryTokenInput
  -- && traceIfFalse "dd" hasGroupInfoTokenFromReferenceInputs
  -- test
  where
    ctx :: StoremanScriptContext
    !ctx = PlutusTx.unsafeFromBuiltinData @StoremanScriptContext rawContext

    info :: TxInfo'
    !info = scriptContextTxInfo' ctx
    
    txInputs :: [TxInInfo']
    !txInputs = txInfoInputs' info

    totalValue :: Value
    !totalValue = go (Plutus.singleton Plutus.adaSymbol Plutus.adaToken 0) txInputs
      where
        go v [] = v
        -- go v (TxInInfo'{txInInfoResolved'=TxOut'{txOutValue'}} : rest) = go (v <>txOutValue') rest
        go v (inInfo : rest) = go (v <> (txOutValue' $ txInInfoResolved' inInfo)) rest

    hasTreasuryTokenInput :: Bool
    !hasTreasuryTokenInput = 
      let !totalInputValue = totalValue -- foldMap (txOutValue' . txInInfoResolved') txInputs
          !amount = valueOf totalInputValue checkTokenSymbol checkTokenName
      in amount == 1



-- typedValidator :: CheckTokenInfoParam -> PV2.TypedValidator TreasuryType
-- typedValidator = PV2.mkTypedValidatorParam @TreasuryType
--     $$(PlutusTx.compile [|| mkValidator ||])
--     $$(PlutusTx.compile [|| wrap ||])
--     where
--         wrap = PV2.mkUntypedValidator

validator :: CheckTokenInfoParam -> Scripts.Validator
validator p = Plutus.mkValidatorScript $
    $$(PlutusTx.compile [|| validatorParam ||])
        `PlutusTx.applyCode`
            PlutusTx.liftCode p
    where validatorParam s = mkUntypedValidator' (mkValidator s)


-- validator :: CheckTokenInfoParam -> Validator
-- validator = PV2.validatorScript . typedValidator

script :: CheckTokenInfoParam -> Plutus.Script
script = Plutus.unValidatorScript . validator

-- treasuryScriptShortBs :: CheckTokenInfoParam -> SBS.ShortByteString
-- treasuryScriptShortBs = SBS.toShort . LBS.toStrict $ serialise . script

-- treasuryScript :: CheckTokenInfoParam -> PlutusScript PlutusScriptV2
-- treasuryScript = PlutusScriptSerialised . treasuryScriptShortBs

treasuryScript :: CheckTokenInfoParam ->  PlutusScript PlutusScriptV2
treasuryScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

treasuryScriptHash :: CheckTokenInfoParam -> Plutus.ValidatorHash
treasuryScriptHash = Scripts.validatorHash . validator

-- treasuryScriptHashStr :: CheckTokenInfoParam -> BuiltinByteString
-- treasuryScriptHashStr = case PlutusTx.fromBuiltinData $ PlutusTx.toBuiltinData . treasuryScriptHash of 
--   Just s -> s
--   Nothing -> ""

treasuryAddress ::CheckTokenInfoParam -> Ledger.Address
treasuryAddress = mkValidatorAddress . validator
