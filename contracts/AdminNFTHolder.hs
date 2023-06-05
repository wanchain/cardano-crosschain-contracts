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

module CrossChain.AdminNFTHolder
  ( adminNFTHolderScript
  , adminNFTHolderScriptHash
  , adminNFTHolderAddress
  -- , AdminDatum (..)
  , AdminActionRedeemer (..)
  ) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($), (&&),(==),(/=),(||),(-),(++),(!!),(>),(>=),(+),(/=),snd,sum ,map,elem,length,filter)
-- import GHC.Generics (Generic)
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





        -- deriving anyclass (ToJSON, FromJSON)

-- instance PlutusTx.Prelude.Eq AdminDatum where
--     {-# INLINABLE (==) #-}
--     AdminDatum signatories minNumSignatures == AdminDatum signatories' minNumSignatures' =
--         signatories == signatories'
--         && minNumSignatures == minNumSignatures'

-- PlutusTx.unstableMakeIsData ''AdminDatum
-- PlutusTx.makeLift ''AdminDatum

data AdminActionRedeemer = Use | Update | Upgrade
    deriving (Show, Prelude.Eq)
PlutusTx.unstableMakeIsData ''AdminActionRedeemer

data Holding
instance Scripts.ValidatorTypes Holding where
    type instance DatumType Holding = ()
    type instance RedeemerType Holding = AdminActionRedeemer

{-# INLINABLE adminDatumEq #-}
adminDatumEq :: AdminDatum -> AdminDatum -> Bool
adminDatumEq a b = ((signatories a) == (signatories b)) && ((minNumSignatures a) == (minNumSignatures b))

-- switch gpk & update admin TODO 
{-# INLINABLE mkValidator #-}
mkValidator :: GroupAdminNFTInfo -> () -> AdminActionRedeemer -> V2.ScriptContext -> Bool
mkValidator (GroupAdminNFTInfo (GroupNFTTokenInfo groupInfoNFTSymbol groupInfoNFTName) (AdminNftTokenInfo adminNFTSymbol adminNFTName)) _ action ctx = 
    traceIfFalse "nau" isAuthorized && 
    traceIfFalse "gmi"  inputHasAdminNFT  && 
    traceIfFalse "gmo"   checkAdminNFTOwnerInOutput   
    && traceIfFalse "wdat" isDatumCorrect
  where 
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    txInfoInputs :: [V2.TxInInfo]
    txInfoInputs = V2.txInfoInputs info

    groupInfo :: GroupInfoParams
    !groupInfo = getGroupInfo info groupInfoNFTSymbol groupInfoNFTName
    
    ownInput :: V2.TxOut
    ownInput = case V2.findOwnInput ctx of
        Nothing -> traceError "mis"
        Just i  -> V2.txInInfoResolved i
    

    isTarget :: V2.TxOut -> Bool
    isTarget V2.TxOut{V2.txOutValue} = (assetClassValueOf txOutValue (assetClass adminNFTSymbol adminNFTName)) == 1


    adminNFTOutput :: V2.TxOut
    adminNFTOutput =
        let 
          -- holder = V2.ownHash ctx
          output = filter isTarget $ V2.txInfoOutputs info
        in  
            case output of
                [o] -> o
                _ -> traceError "mto"
  
    groupInfoParams:: V2.TxOut->GroupInfoParams
    groupInfoParams (V2.TxOut _ _ txOutDatum _) =
      case txOutDatum of
        (Plutus.OutputDatum datum ) -> case Plutus.fromBuiltinData $ Plutus.getDatum datum of 
          Just params -> params
          _ -> traceError "wd"
        _ -> traceError "bdt"

    adminDatum:: V2.TxOut-> AdminDatum
    adminDatum (V2.TxOut _ _ txOutDatum _) =
      case txOutDatum of
        (Plutus.OutputDatum datum ) -> case Plutus.fromBuiltinData $ Plutus.getDatum datum of 
          Just params -> params
          _ -> traceError "wd2"
        _ -> traceError "bdt2"
    
    adminNFTValue :: V2.TxOut -> Integer
    adminNFTValue o = assetClassValueOf (V2.txOutValue o) (assetClass adminNFTSymbol adminNFTName)

    isSignedBy :: BuiltinByteString -> Bool
    isSignedBy pkh = V2.txSignedBy info (PubKeyHash pkh)

    isAuthorized :: Bool
    isAuthorized = 
        let adminDatumInfo = adminDatum ownInput
            present = length (filter isSignedBy $ signatories adminDatumInfo)
         in (present >= minNumSignatures adminDatumInfo)

    inputHasAdminNFT :: Bool
    inputHasAdminNFT =  (adminNFTValue ownInput) == 1
            
    checkAdminNFTOwnerInOutput :: Bool
    checkAdminNFTOwnerInOutput = case adminNFTOutput of
      V2.TxOut{V2.txOutAddress=Address (Plutus.ScriptCredential s) _} -> 
        case action of -- owner must be the same with before
          Update -> s == (V2.ownHash ctx)
          Use -> s == (V2.ownHash ctx)
          Upgrade -> s /= (V2.ownHash ctx) -- owner must be different with before

    checkNewDatum :: Bool
    checkNewDatum = 
        let adminDatumInfo = adminDatum adminNFTOutput 
            minCount = minNumSignatures adminDatumInfo
            totalCount = length $ signatories adminDatumInfo
        in (totalCount >= minCount) && (minCount > 0)
    
    -- hasDatumInOutput :: V2.TxOut -> Bool
    -- hasDatumInOutput V2.TxOut{V2.txOutAddress, V2.txOutDatum} = case txOutAddress of
    --   (Address (Plutus.ScriptCredential s) _) -> case txOutDatum of
    --     (Plutus.OutputDatum datum ) -> case Plutus.fromBuiltinData $ Plutus.getDatum datum of 
    --       Just params -> True
    --       _ -> False
    --     _ -> False
    --   _ -> True

    isDatumCorrect :: Bool
    isDatumCorrect = case action of
      Use -> adminDatumEq (adminDatum ownInput) (adminDatum adminNFTOutput)
      Update -> checkNewDatum
      Upgrade -> case adminNFTOutput of
        V2.TxOut{V2.txOutAddress, V2.txOutDatum} -> case txOutAddress of
          (Address (Plutus.ScriptCredential s) _) -> case txOutDatum of
            (Plutus.OutputDatum datum ) -> True
            _ -> False
          (Address (Plutus.PubKeyCredential k) _) -> True


    
      


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

adminNFTHolderScriptShortBs :: GroupAdminNFTInfo -> SBS.ShortByteString
adminNFTHolderScriptShortBs c = SBS.toShort . LBS.toStrict $ serialise  (script c)

adminNFTHolderScript :: GroupAdminNFTInfo ->  PlutusScript PlutusScriptV2
-- adminNFTHolderScript = PlutusScriptSerialised . groupInfoTokenHolderScriptShortBs
adminNFTHolderScript c = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script c)


adminNFTHolderAddress :: GroupAdminNFTInfo -> Ledger.Address
adminNFTHolderAddress = PV2.validatorAddress . typedValidator

adminNFTHolderScriptHash :: GroupAdminNFTInfo -> ValidatorHash
adminNFTHolderScriptHash = PV2.validatorHash .typedValidator