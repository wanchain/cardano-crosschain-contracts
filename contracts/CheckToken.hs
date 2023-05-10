
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


module CrossChain.CheckToken
  ( checkTokenScript
  , checkTokenScriptShortBs
  , checkTokenCurrency
  , CheckTokenParam
  , CheckTokenParam (..)
  ) where


import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&), (&&), (==),(-),(++),(<),(||),not,length,filter,(>),mconcat,map,head,any,all,snd,(!!))

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

-- import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)
import Plutus.V1.Ledger.Value
-- import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes))
import Ledger.Crypto (PubKey (..), PubKeyHash (..))
import Data.Aeson (FromJSON, ToJSON)
import PlutusTx (BuiltinData, CompiledCode, Lift, applyCode, liftCode, fromData)
import GHC.Generics (Generic)
import Plutus.Script.Utils.Typed (validatorScript,validatorAddress,validatorHash)
-- import Plutus.V1.Ledger.Scripts (unValidatorScript)
import Plutus.V1.Ledger.Value (valueOf,flattenValue,assetClass)
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import CrossChain.Types 
-- import Plutus.V2.Ledger.Tx (isPayToScriptOut)

-- import PlutusTx.Builtins (decodeUtf8,sha3_256,appendByteString)

data CheckTokenParam
  = CheckTokenParam
      { groupInfoNFTCurrency :: CurrencySymbol
        , groupInfoNFTName :: TokenName
        , checkTokenName :: TokenName
        , groupInfoIndex :: ParamType
      } deriving stock (Generic)
        -- deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''CheckTokenParam
PlutusTx.makeLift ''CheckTokenParam


{-# INLINABLE mkPolicy #-}
mkPolicy :: CheckTokenParam -> () -> ScriptContext -> Bool
mkPolicy (CheckTokenParam groupInfoNFTCurrency groupInfoNFTName checkTokenName groupInfoIndex) () ctx = 
  traceIfFalse "wa" checkMint
  && traceIfFalse "s" (V2.txSignedBy info  (PubKeyHash  (getGroupInfoParams groupInfo Admin)))
  -- && traceIfFalse "eo" checkOutput
  -- && traceIfFalse "m" checkOutputCount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    isGroupInfoToken :: V2.TxOut -> Bool
    isGroupInfoToken (V2.TxOut (V2.Address credential _) txOutValue _ _) = (assetClassValueOf txOutValue ( assetClass groupInfoNFTCurrency groupInfoNFTName)) > 0

    refUtxosOfGroupInfo :: [V2.TxOut]
    refUtxosOfGroupInfo = map (V2.txInInfoResolved) $ txInfoReferenceInputs info

    getGroupInfoTokenFromReferenceInputs :: V2.TxOut
    getGroupInfoTokenFromReferenceInputs = case filter (isGroupInfoToken) refUtxosOfGroupInfo of
        [o] -> o
    
    groupInfo :: GroupInfoParams
    groupInfo = 
      case getGroupInfoTokenFromReferenceInputs of
        (V2.TxOut _ _ outputDatum _) -> case outputDatum of
          (V2.OutputDatum datum ) -> case (V2.fromBuiltinData $ V2.getDatum datum) of
            Just groupInfo -> groupInfo

    mintInfo :: (CurrencySymbol, TokenName, Integer)
    mintInfo = case flattenValue (txInfoMint info) of
        [(symbols, name, amt)] -> (symbols, name, amt) -- symbols == (ownCurrencySymbol ctx) && name == checkTokenName && (amt == 1 || amt == (-1))
        -- _               -> False
    checkMint :: Bool
    checkMint = 
      let (symbols,name,amt) = mintInfo
      in symbols == (ownCurrencySymbol ctx) && name == checkTokenName && ( (amt > 0 && checkOutput) || (amt < 0))


    checkOutput :: Bool
    checkOutput = 
      let 
        to = V2.ValidatorHash (getGroupInfoParams groupInfo groupInfoIndex)
        outValues = map (snd) $ V2.scriptOutputsAt to info
        outTotalValue = mconcat outValues
        outAmount = valueOf outTotalValue (ownCurrencySymbol ctx) checkTokenName
        (_,_,mintAmount) = mintInfo
      in mintAmount == outAmount && length outValues == mintAmount



policy :: CheckTokenParam -> V2.MintingPolicy
policy oref = V2.mkMintingPolicyScript $ $$(PlutusTx.compile [|| \c -> V2.mkUntypedMintingPolicy (mkPolicy c)  ||]) 
    `PlutusTx.applyCode` PlutusTx.liftCode oref


checkTokenCurrency :: CheckTokenParam -> CurrencySymbol
checkTokenCurrency = scriptCurrencySymbol . policy

plutusScript :: CheckTokenParam -> V2.Script
plutusScript = V2.unMintingPolicyScript . policy

validator :: CheckTokenParam -> V2.Validator
validator = V2.Validator . plutusScript

scriptAsCbor :: CheckTokenParam -> LBS.ByteString
scriptAsCbor = serialise . validator

checkTokenScript :: CheckTokenParam -> PlutusScript PlutusScriptV2
checkTokenScript mgrData = PlutusScriptSerialised . SBS.toShort $ LBS.toStrict (scriptAsCbor mgrData)

checkTokenScriptShortBs :: CheckTokenParam -> SBS.ShortByteString
checkTokenScriptShortBs mgrData = SBS.toShort . LBS.toStrict $ (scriptAsCbor mgrData)