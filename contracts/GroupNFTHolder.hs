{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}

module CrossChain.GroupNFTHolder
  ( groupNFTHolderScript
  ,groupNFTHolderScriptHash
  ,groupNFTHolderAddress
  , GroupInfoParams (..)
  -- ,GroupAdminNFTInfo (..)
  ,ParamType (..)
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&),(==),(||),(-),(++),(!!),(>),(>=),(+),(/=),snd,sum ,map,elem,length,filter)

import Codec.Serialise
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Short qualified as SBS
import Plutus.V1.Ledger.Value
-- import Plutus.Script.Utils.V2.Scripts as Scripts
import Ledger.Address (PaymentPrivateKey (PaymentPrivateKey, unPaymentPrivateKey), PaymentPubKey (PaymentPubKey),PaymentPubKeyHash (..),unPaymentPubKeyHash,toPubKeyHash,toValidatorHash)
import Plutus.V2.Ledger.Api qualified as Plutus
import Plutus.V2.Ledger.Contexts as V2
import Plutus.Script.Utils.V2.Typed.Scripts qualified as PV2

import PlutusTx qualified
-- import PlutusTx.Builtins
import PlutusTx.Builtins
-- import PlutusTx.Eq as PlutusTx
-- import PlutusTx.Eq()
import PlutusTx.Prelude hiding (SemigroupInfo (..), unless, (.))
-- import PlutusTx.Prelude qualified as PlutusPrelude
import           Ledger               hiding (singleton,validatorHash)
-- import Ledger.Typed.Scripts qualified as Scripts
import Plutus.V1.Ledger.Scripts (getDatum)
-- import Plutus.Script.Utils.Typed (validatorScript,validatorAddress,validatorHash)
import Plutus.V1.Ledger.Scripts (unValidatorScript)
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import CrossChain.Types



data Holding
instance Scripts.ValidatorTypes Holding where
    type instance DatumType Holding = ()
    type instance RedeemerType Holding = Integer

-- switch gpk & update admin TODO 
{-# INLINABLE mkValidator #-}
mkValidator :: GroupAdminNFTInfo -> () -> Integer -> V2.ScriptContext -> Bool
mkValidator (GroupAdminNFTInfo (GroupNFTTokenInfo groupNftSymbol groupNftName) (AdminNftTokenInfo adminNftSymbol adminNftName)) _ action ctx = traceIfFalse "noauth" isAuthorized
  && traceIfFalse "gmi"  inputHasToken  
  && traceIfFalse "gmo"   outputHasToken --(outputHasToken && isRigthOWner) -- || (checkMintedOrBurnAmount $ negate 1))
  && traceIfFalse "wdat" checkNewDatum
  where 
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    txInfoInputs :: [V2.TxInInfo]
    txInfoInputs = V2.txInfoInputs info
    
    hasAdminNftInInput :: Bool
    !hasAdminNftInInput = 
      let !totalInputValue = V2.valueSpent info
          !amount = valueOf totalInputValue adminNftSymbol adminNftName
      in amount == 1

    --only admin has permission
    isAuthorized :: Bool
    isAuthorized = 
      let !oracleWorkerPKH = getGroupInfoParams (groupInfoParams ownInput) OracleWorker
      in (hasAdminNftInInput) || ((action == 2) && (V2.txSignedBy info  (PubKeyHash oracleWorkerPKH)))
    
    ownInput :: V2.TxOut
    ownInput = case V2.findOwnInput ctx of
        Nothing -> traceError "groupInfotoken input missing"
        Just i  -> V2.txInInfoResolved i

    groupTokenValue :: V2.TxOut -> Integer
    groupTokenValue o = assetClassValueOf (V2.txOutValue o) (assetClass groupNftSymbol groupNftName)

    inputHasToken :: Bool
    inputHasToken =  (groupTokenValue ownInput) > 0

    isTargetValidatorHash:: V2.TxOut -> Bool
    -- isTargetValidatorHash V2.TxOut{V2.txOutAddress=Address (Plutus.ScriptCredential s) _} = s == (ValidatorHash ( getGroupInfoParams (groupInfoParams ownInput) Version) )
    isTargetValidatorHash o = (groupTokenValue o) > 0

    lockedByTarget :: [V2.TxOut]
    lockedByTarget = filter (isTargetValidatorHash) $ V2.txInfoOutputs info
      -- [o] -> o

    ownOutput :: V2.TxOut
    ownOutput  
      | action > 0 = 
        case V2.getContinuingOutputs ctx of
          [o] -> o  
          _   -> traceError "expected exactly one groupInfotoken output"
      | action == 0 = 
        case lockedByTarget of
          [o] -> o
          _   -> traceError "expected exactly one groupInfotoken output"

    groupInfoParams:: V2.TxOut->GroupInfoParams
    groupInfoParams (V2.TxOut _ _ txOutDatum _) =
      case txOutDatum of
        (Plutus.OutputDatum datum ) -> case Plutus.fromBuiltinData $ Plutus.getDatum datum of 
          Just params -> params
          _ -> traceError "wrong datum"
        _ -> traceError "bad datum type"
            
    outputHasToken :: Bool
    outputHasToken = -- True
      let params = groupInfoParams ownOutput
          ver = getGroupInfoParams params Version
      in  
        case V2.txOutAddress ownOutput of
          Address (Plutus.ScriptCredential s) _ -> s == (ValidatorHash ver)
          _ -> False

    paramsDiff :: [BuiltinByteString] -> [BuiltinByteString] -> Integer -> Bool
    paramsDiff oldP newP i = go oldP newP 0
      where
        go [] [] _ = True
        go (old:ro) [] _ = False
        go [] (new:rn) _ = True
        go (old:ro) (new:rn) n = 
          if (old /= new)  && (n /= i) then False
          else go ro rn (n+1)
    
    checkNewDatum :: Bool
    checkNewDatum =
      let 
        !oldParams = params $ groupInfoParams ownInput
        !newParams = params $ groupInfoParams ownOutput
        -- !flags =  map (\p -> if elem p newParams then 1 else 0 ) oldParams 
        -- !cnt = sum flags
      in paramsDiff oldParams newParams action
      


typedValidator :: GroupAdminNFTInfo -> PV2.TypedValidator Holding
typedValidator = PV2.mkTypedValidatorParam @Holding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator

validator :: GroupAdminNFTInfo -> Validator
validator = PV2.validatorScript . typedValidator

script :: GroupAdminNFTInfo -> Plutus.Script
script = unValidatorScript . validator

groupInfoTokenHolderScriptShortBs :: GroupAdminNFTInfo -> SBS.ShortByteString
groupInfoTokenHolderScriptShortBs c = SBS.toShort . LBS.toStrict $ serialise  (script c)

groupNFTHolderScript :: GroupAdminNFTInfo ->  PlutusScript PlutusScriptV2
-- groupNFTHolderScript = PlutusScriptSerialised . groupInfoTokenHolderScriptShortBs
groupNFTHolderScript c = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script c)


groupNFTHolderAddress :: GroupAdminNFTInfo -> Ledger.Address
groupNFTHolderAddress = PV2.validatorAddress . typedValidator

groupNFTHolderScriptHash :: GroupAdminNFTInfo -> ValidatorHash
groupNFTHolderScriptHash = PV2.validatorHash .typedValidator