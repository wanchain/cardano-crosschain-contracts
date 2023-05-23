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

module CrossChain.StakeCheck
  ( stakeCheckScript
  ,stakeCheckScriptHash
  ,stakeCheckAddress
--   , GroupAdminNFTInfo (..)
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
-- import Plutus.V1.Ledger.Scripts (getDatum)

data StakeCheckRedeemer = WithdrawU | SpendU
    deriving (Show, Prelude.Eq)
PlutusTx.unstableMakeIsData ''StakeCheckRedeemer

data StakeCheckType
instance Scripts.ValidatorTypes StakeCheckType where
    type instance DatumType StakeCheckType = ()
    type instance RedeemerType StakeCheckType = StakeCheckRedeemer



{-# INLINABLE mkValidator #-}
mkValidator :: GroupAdminNFTInfo ->() -> StakeCheckRedeemer -> V2.ScriptContext -> Bool
mkValidator (GroupAdminNFTInfo (GroupNFTTokenInfo groupInfoNFTSymbol groupInfoNFTName) (AdminNftTokenInfo adminNFTSymbol adminNFTName)) _ redeemer ctx = 
  traceIfFalse "auth" hasAuthorized  &&
  traceIfFalse "hout" checkOwnOutput 
  where 
    info :: V2.TxInfo
    !info = V2.scriptContextTxInfo ctx

    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoNFTSymbol groupInfoNFTName

    isDatum:: OutputDatum -> Bool
    isDatum d = case d of
        OutputDatum datum -> True
        _ -> False
    
    isOnlyAda :: Value -> Bool
    isOnlyAda v = 
        let adaAmount = valueOf v Ada.adaSymbol Ada.adaToken
            newValue = Plutus.singleton Ada.adaSymbol Ada.adaToken adaAmount
        in  v == newValue
    
    hasOwnOutput :: Bool
    hasOwnOutput = case V2.getContinuingOutputs ctx of
        [o] -> case o of
            V2.TxOut{V2.txOutDatum=d, V2.txOutValue} ->  
                if (isDatum d) && (isOnlyAda txOutValue) then True
                else False
            _ -> False
        _ -> False
    
    checkOwnOutput :: Bool
    checkOwnOutput = case redeemer of
        WithdrawU -> True
        SpendU -> hasOwnOutput
    
    hasAuthorized :: Bool
    hasAuthorized = 
        let totalInputValue = V2.valueSpent info
            adminNftAmount = valueOf totalInputValue adminNFTSymbol adminNFTName
        in adminNftAmount == 1

  

typedValidator :: GroupAdminNFTInfo -> PV2.TypedValidator StakeCheckType
typedValidator = PV2.mkTypedValidatorParam @StakeCheckType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator


validator :: GroupAdminNFTInfo -> Validator
validator = PV2.validatorScript . typedValidator

script :: GroupAdminNFTInfo -> Plutus.Script
script = unValidatorScript . validator

stakeCheckScript :: GroupAdminNFTInfo ->  PlutusScript PlutusScriptV2
stakeCheckScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

stakeCheckScriptHash :: GroupAdminNFTInfo -> Plutus.ValidatorHash
stakeCheckScriptHash = PV2.validatorHash .typedValidator

stakeCheckAddress ::GroupAdminNFTInfo -> Ledger.Address
stakeCheckAddress = PV2.validatorAddress . typedValidator
