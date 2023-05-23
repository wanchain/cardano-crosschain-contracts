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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:dump-uplc #-}

module CrossChain.TreasuryCheck
  ( treasuryCheckScript
  -- , authorityCheckScriptShortBs
  ,treasuryCheckScriptHash
  -- ,authorityCheckScriptHashStr
  ,treasuryCheckAddress
  , TreasuryCheckProof (..)
  -- ,TreasuryCheckProof
  , TreasuryCheckRedeemer(..)
  ,TreasuryCheckParams (..)
  ,TreasuryCheckParams
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($),(<>), (&&), (&&), (==),(||),(>=),(<=),(<),(-),(/=),not,length,filter,(>),(+),map,head,reverse,any,elem,snd,mconcat,negate,all)

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
import Plutus.V1.Ledger.Value (valueOf,currencySymbol,tokenName,symbols,flattenValue,assetClass)
import PlutusTx.Builtins --(decodeUtf8,sha3_256,appendByteString)
import Ledger.Address 
import Ledger.Value
import Plutus.V2.Ledger.Contexts as V2
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import Plutus.V1.Ledger.Tx
import CrossChain.Types 
-- ===================================================
-- import Plutus.V1.Ledger.Value
-- import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)

import Ledger hiding (validatorHash) --singleton




data TreasuryCheckProof = TreasuryCheckProof
  {
    toPkhPay :: BuiltinByteString -- send toPkh 
    , toPkhStk :: BuiltinByteString
    , policy :: BuiltinByteString -- which token , zero indicated only transfer ada
    , assetName :: BuiltinByteString
    , amount :: Integer  -- token amount
    , adaAmount :: Integer -- addtional ada amount
    , txHash :: BuiltinByteString
    , index :: Integer
    , mode :: Integer
    , uniqueId :: BuiltinByteString
    , txType :: Integer
    , ttl :: Integer
    , outputCount :: Integer
    , signature :: BuiltinByteString
  }deriving (Prelude.Eq, Show)


PlutusTx.unstableMakeIsData ''TreasuryCheckProof
PlutusTx.makeLift ''TreasuryCheckProof

data TreasuryCheckRedeemer = BurnTreasuryCheckToken | TreasuryCheckRedeemer TreasuryCheckProof
    deriving (Show, Prelude.Eq)
PlutusTx.unstableMakeIsData ''TreasuryCheckRedeemer

data TreasuryType
instance Scripts.ValidatorTypes TreasuryType where
    type instance DatumType TreasuryType = ()
    type instance RedeemerType TreasuryType = TreasuryCheckRedeemer

data TreasuryCheckParams
  = TreasuryCheckParams
      { tokenInfos :: GroupAdminNFTCheckTokenInfo
        , treasury :: ValidatorHash 
      } deriving (Generic, Prelude.Eq)
        -- deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''TreasuryCheckParams
PlutusTx.makeLift ''TreasuryCheckParams


{-# INLINABLE burnTokenCheck #-}
burnTokenCheck :: TreasuryCheckParams -> V2.ScriptContext -> Bool
burnTokenCheck (TreasuryCheckParams (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) treasury) ctx = 
  traceIfFalse "a" hasAdminNftInInput 
  && traceIfFalse "b" checkOutPut
  && traceIfFalse "ti" (not hasTreasuryInput)
  where 
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

    hasAdminNftInInput :: Bool
    !hasAdminNftInInput = 
      let !totalInputValue = V2.valueSpent info
          !amount = valueOf totalInputValue adminNftSymbol adminNftName
      in amount == 1

    checkOutPut :: Bool
    !checkOutPut = 
      let !totalAmountOfCheckTokenInOutput = getAmountOfCheckTokenInOutput ctx checkTokenSymbol checkTokenName
          !outputsAtChecker = map snd $ scriptOutputsAt' (ValidatorHash (getGroupInfoParams groupInfo TreasuryCheckVH)) (getGroupInfoParams groupInfo StkVh) info
          !outputAtCheckerSum = valueOf (mconcat outputsAtChecker) checkTokenSymbol checkTokenName
      in totalAmountOfCheckTokenInOutput == outputAtCheckerSum && (length outputsAtChecker) == outputAtCheckerSum


    hasTreasuryInput :: Bool
    !hasTreasuryInput = any (\V2.TxOut{V2.txOutAddress=Address (Plutus.ScriptCredential s) _} -> s == treasury) $ map V2.txInInfoResolved $ V2.txInfoInputs info

{-# INLINABLE treasurySpendCheck #-}
treasurySpendCheck :: TreasuryCheckParams -> TreasuryCheckProof-> V2.ScriptContext -> Bool
treasurySpendCheck (TreasuryCheckParams (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) treasury) (TreasuryCheckProof toPkhPay toPkhStk policy assetName amount adaAmount txHash index mode uniqueId txType ttl outputCount signature) ctx = 
  -- traceIfFalse "l" (V2.txSignedBy info  (PubKeyHash  (getGroupInfoParams groupInfo BalanceWorker))) && 
  traceIfFalse "ht" (hasUTxO ctx) && 
  traceIfFalse "hot" (amountOfCheckTokeninOwnOutput == 1) && 
  traceIfFalse "cs" checkSignature && 
  traceIfFalse "tt" checkTxInOut  && -- check asset balance between input and output
  traceIfFalse "notr" hasTreasuryInput && -- check has treasury input 
  traceIfFalse "ttl" checkTtl
  where 
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    hashRedeemer :: BuiltinByteString
    !hashRedeemer = 
        let !tmp1 = appendByteString (appendByteString (appendByteString (appendByteString (appendByteString toPkhPay toPkhStk) policy) assetName) (packInteger amount)) (packInteger adaAmount)
            !tmp2 = appendByteString (appendByteString (appendByteString tmp1 txHash) (packInteger index)) (packInteger mode)
            !tmp3 = appendByteString tmp2 uniqueId
            !tmp4 = appendByteString tmp3 (packInteger txType)
            !tmp5 = appendByteString tmp4 (packInteger ttl)
            !tmp6 = appendByteString tmp5 (packInteger outputCount)
        in sha3_256 tmp6
    
    hasUTxO :: V2.ScriptContext -> Bool
    hasUTxO V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} = (V2.txOutRefId txOutRef) == (Plutus.TxId txHash) && (V2.txOutRefIdx txOutRef) == index

    verify :: Integer -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString-> Bool
    verify mode pk hash signatureGroupInfoParams
      | mode == 0 = verifyEcdsaSecp256k1Signature pk hash signature
      | mode == 1 = verifySchnorrSecp256k1Signature pk hash signature
      | mode == 2 = verifyEd25519Signature pk hash signature
      -- | otherwise = traceError "m"
    
    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoCurrency groupInfoTokenName

    amountOfCheckTokeninOwnOutput :: Integer
    !amountOfCheckTokeninOwnOutput = getAmountOfCheckTokeninOwnOutput ctx checkTokenSymbol checkTokenName (getGroupInfoParams groupInfo StkVh)

    checkSignature :: Bool
    !checkSignature = 
      let !groupInfoPk = getGroupInfoParams groupInfo GPK
      in verify mode groupInfoPk hashRedeemer signature

    isTreasuryInput:: V2.TxInInfo -> Bool
    isTreasuryInput (V2.TxInInfo _ (V2.TxOut (Address addressCredential _) _ _ _)) = 
      case addressCredential of
        (Plutus.ScriptCredential s) -> s == treasury
        _ -> False
        

    treasuryInputValue :: Value
    !treasuryInputValue = go (Plutus.singleton Plutus.adaSymbol Plutus.adaToken 0) (V2.txInfoInputs info)
      where
        go v [] = v
        go v (V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutValue,V2.txOutAddress}} : rest) = case txOutAddress of
          Address{addressCredential} -> case addressCredential of
            Plutus.ScriptCredential s -> 
              if s == treasury then 
                if isValidValue txOutValue then go (v <> txOutValue) rest
                else traceError "bi"
              else go v rest
            _ -> go v rest

    hasTreasuryInput :: Bool
    !hasTreasuryInput = ((valueOf treasuryInputValue Ada.adaSymbol Ada.adaToken) > 0)

    targetSymbol :: CurrencySymbol 
    !targetSymbol = CurrencySymbol policy
    
    targetTokenName :: TokenName
    !targetTokenName = TokenName assetName

    valuePaidToTarget :: Value
    !valuePaidToTarget 
      | txType == 1 = valueLockedBy' info treasury (getGroupInfoParams groupInfo StkVh)
        -- let outValues = scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info
        -- in 
        --   if any (\v -> not $ isSingleAsset v targetSymbol targetTokenName) outValues then traceError "bo"
        --   else mconcat outValues
      | otherwise = valuePaidTo' info (PubKeyHash toPkhPay) toPkhStk


    isExpectedValue :: Value -> CurrencySymbol -> TokenName -> Bool
    isExpectedValue v cs tk = 
      if cs == Ada.adaSymbol && tk == Ada.adaToken then v == Plutus.singleton Plutus.adaSymbol Plutus.adaToken assetAmount
      else (v == ((Plutus.singleton Plutus.adaSymbol Plutus.adaToken (valueOf v Ada.adaSymbol Ada.adaToken)) <> Plutus.singleton cs tk assetAmount)) 
      && (assetAmount > 0)
      where
        assetAmount = valueOf v cs tk

    -- isSingleAsset :: Value -> CurrencySymbol -> TokenName -> Bool
    -- isSingleAsset v cs tk = not $ any (\(cs',tk',_) -> cs' /= cs && cs' /= Ada.adaSymbol && tk' /= tk && tk' /= Ada.adaToken) $ flattenValue v

    isMultiAsset :: Value ->Bool
    isMultiAsset v = (length $ flattenValue v) > 2

    isValidValue :: Value -> Bool
    isValidValue v
      | txType == 2 = isMultiAsset v
      | otherwise = isExpectedValue v targetSymbol targetTokenName

    crossValue :: Value
    !crossValue
      | txType == 0 = Ada.lovelaceValueOf adaAmount <> Plutus.singleton targetSymbol targetTokenName amount
      | otherwise = Ada.lovelaceValueOf 0


    -- 1. 
    checkTx :: Bool 
    !checkTx = 
        let !receivedValue = valuePaidToTarget -- V2.valuePaidTo info (PubKeyHash toPkhPay)
            !inputValue = treasuryInputValue
            !changeValues = map snd $ scriptOutputsAt' treasury (getGroupInfoParams groupInfo StkVh) info
            !remainValue = mconcat changeValues
            !valueSum = crossValue <> remainValue
        in 
          (receivedValue `geq` crossValue) && (valueSum `geq` inputValue) && (length changeValues == outputCount) 
          && (isSingleAsset remainValue targetSymbol targetTokenName) 

    checkTxInOut:: Bool
    !checkTxInOut  
      | txType == 0 = checkTx
      | txType == 1 = ((ValidatorHash toPkhPay) == treasury ) && (toPkhStk == (getGroupInfoParams groupInfo StkVh)) && checkTx
      | txType == 2 = (valuePaidTo' info (PubKeyHash toPkhPay) toPkhStk ) `geq` treasuryInputValue

    checkTtl :: Bool
    !checkTtl = 
      let !range = V2.txInfoValidRange info
          !ttlRange = to (Plutus.POSIXTime ttl)
      in ttlRange == range 

{-# INLINABLE mkValidator #-}
mkValidator :: TreasuryCheckParams ->() -> TreasuryCheckRedeemer -> V2.ScriptContext -> Bool
mkValidator storeman _ redeemer ctx = 
  case redeemer of
    BurnTreasuryCheckToken -> burnTokenCheck storeman ctx
    TreasuryCheckRedeemer treasuryRedeemer -> treasurySpendCheck storeman treasuryRedeemer ctx


typedValidator :: TreasuryCheckParams -> PV2.TypedValidator TreasuryType
typedValidator = PV2.mkTypedValidatorParam @TreasuryType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator


validator :: TreasuryCheckParams -> Validator
validator = PV2.validatorScript . typedValidator

script :: TreasuryCheckParams -> Plutus.Script
script = unValidatorScript . validator

-- authorityCheckScriptShortBs :: TreasuryCheckParams -> SBS.ShortByteString
-- authorityCheckScriptShortBs = SBS.toShort . LBS.toStrict $ serialise . script

-- treasuryCheckScript :: CurrencySymbol -> PlutusScript PlutusScriptV2
-- treasuryCheckScript = PlutusScriptSerialised . authorityCheckScriptShortBs

treasuryCheckScript :: TreasuryCheckParams ->  PlutusScript PlutusScriptV2
treasuryCheckScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

treasuryCheckScriptHash :: TreasuryCheckParams -> Plutus.ValidatorHash
treasuryCheckScriptHash = PV2.validatorHash .typedValidator

-- authorityCheckScriptHashStr :: TreasuryCheckParams -> BuiltinByteString
-- authorityCheckScriptHashStr = case PlutusTx.fromBuiltinData $ PlutusTx.toBuiltinData . treasuryCheckScriptHash of 
--   Just s -> s
--   Nothing -> ""

treasuryCheckAddress ::TreasuryCheckParams -> Ledger.Address
treasuryCheckAddress = PV2.validatorAddress . typedValidator
