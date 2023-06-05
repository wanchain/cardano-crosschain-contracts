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

module CrossChain.MintCheck
  ( mintCheckScript
  -- , authorityCheckScriptShortBs
  ,mintCheckScriptHash
  -- ,authorityCheckScriptHashStr
  ,mintCheckAddress
  , MintCheckProof (..)
  , MintCheckRedeemer (..)
  -- , GroupAdminNFTCheckTokenInfo (..)
  -- , GroupAdminNFTCheckTokenInfo
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($),(<>), (&&), (&&), (==),(||),(>=),(<=),(+),(<),(-),not,length,filter,(>),(!!),map,head,reverse,any,elem,snd,mconcat,negate,all)

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




data MintCheckProof = MintCheckProof
  {
    toPkhPay :: BuiltinByteString -- send to 
    , toPkhStk :: BuiltinByteString
    , policy :: BuiltinByteString -- which token , zero indicated only transfer ada
    , assetName :: BuiltinByteString
    , amount :: Integer  -- token amount
    -- , adaAmount :: Integer -- addtional ada amount
    , txHash :: BuiltinByteString
    , index :: Integer
    , mode :: Integer
    , uniqueId :: BuiltinByteString
    -- , txType :: Integer
    , ttl :: Integer
    , signature :: BuiltinByteString
  }deriving (Prelude.Eq, Show)


PlutusTx.unstableMakeIsData ''MintCheckProof
PlutusTx.makeLift ''MintCheckProof

data MintCheckRedeemer = BurnMintCheckToken | MintCheckRedeemer MintCheckProof
    deriving (Show, Prelude.Eq)
PlutusTx.unstableMakeIsData ''MintCheckRedeemer


data TreasuryType
instance Scripts.ValidatorTypes TreasuryType where
    type instance DatumType TreasuryType = ()
    type instance RedeemerType TreasuryType = MintCheckRedeemer

-- data GroupAdminNFTCheckTokenInfo
--   = GroupAdminNFTCheckTokenInfo
--       { groupNftInfo :: GroupNFTTokenInfo
--         , adminNft :: AdminNftTokenInfo
--         , checkToken:: CheckTokenInfo
--       } deriving stock (Generic)
--         deriving anyclass (ToJSON, FromJSON)

-- PlutusTx.unstableMakeIsData ''GroupAdminNFTCheckTokenInfo
-- PlutusTx.makeLift ''GroupAdminNFTCheckTokenInfo

{-# INLINABLE burnTokenCheck #-}
burnTokenCheck :: GroupAdminNFTCheckTokenInfo -> V2.ScriptContext -> Bool
burnTokenCheck (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) ctx = 
  traceIfFalse "a"  hasAdminNftInInput
  && traceIfFalse "b" checkOutPut
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
          !outputsAtChecker = map snd $ scriptOutputsAt' (ValidatorHash (getGroupInfoParams groupInfo MintCheckVH)) (getGroupInfoParams groupInfo StkVh) info
          !outputAtCheckerSum = valueOf (mconcat outputsAtChecker) checkTokenSymbol checkTokenName
      in totalAmountOfCheckTokenInOutput == outputAtCheckerSum && (length outputsAtChecker) == outputAtCheckerSum



{-# INLINABLE mintSpendCheck #-}
mintSpendCheck :: GroupAdminNFTCheckTokenInfo -> MintCheckProof -> V2.ScriptContext -> Bool
mintSpendCheck (GroupAdminNFTCheckTokenInfo (GroupNFTTokenInfo groupInfoCurrency groupInfoTokenName) (AdminNftTokenInfo adminNftSymbol adminNftName) (CheckTokenInfo checkTokenSymbol checkTokenName)) (MintCheckProof toPkhPay toPkhStk policy assetName amount txHash index mode uniqueId ttl signature) ctx = 
  -- traceIfFalse "l" (V2.txSignedBy info  (PubKeyHash  (getGroupInfoParams groupInfo BalanceWorker))) && 
  traceIfFalse "hm" (hasUTxO ctx) && 
  traceIfFalse "om" (amountOfCheckTokeninOwnOutput == 1) && 
  traceIfFalse "am" checkSignature &&  
  traceIfFalse "m" checkMint && 
  traceIfFalse "txm" checkTx &&
  traceIfFalse "ttl" checkTtl
  where
    
    info :: V2.TxInfo
    info = V2.scriptContextTxInfo ctx

    isGroupInfoToken :: V2.TxOut -> Bool
    isGroupInfoToken (V2.TxOut (Address credential _) txOutValue _ _) = (assetClassValueOf txOutValue ( assetClass groupInfoCurrency groupInfoTokenName)) > 0

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
          (Plutus.OutputDatum datum ) -> case (Plutus.fromBuiltinData $ Plutus.getDatum datum) of
            Just groupInfo -> groupInfo

    hasUTxO :: V2.ScriptContext -> Bool
    hasUTxO V2.ScriptContext{V2.scriptContextPurpose=Spending txOutRef} = (V2.txOutRefId txOutRef) == (Plutus.TxId txHash) && (V2.txOutRefIdx txOutRef) == index


    amountOfCheckTokeninOwnOutput :: Integer
    !amountOfCheckTokeninOwnOutput = getAmountOfCheckTokeninOwnOutput ctx checkTokenSymbol checkTokenName (getGroupInfoParams groupInfo StkVh)


    hashRedeemer :: BuiltinByteString
    hashRedeemer = 
        let !tmp1 = appendByteString (appendByteString (appendByteString (appendByteString toPkhPay toPkhStk) policy) assetName) (packInteger amount) --(packInteger adaAmount)
            !tmp2 = appendByteString (appendByteString (appendByteString tmp1 txHash) (packInteger index)) (packInteger mode)
            !tmp3 = appendByteString tmp2 uniqueId
            !tmp4 = appendByteString tmp3 (packInteger ttl)
        in sha3_256 tmp4
    
    
    verify :: Integer -> BuiltinByteString -> BuiltinByteString -> BuiltinByteString-> Bool
    verify mode pk hash signature
      | mode == 0 = verifyEcdsaSecp256k1Signature pk hash signature
      | mode == 1 = verifySchnorrSecp256k1Signature pk hash signature
      | mode == 2 = verifyEd25519Signature pk hash signature
      -- | otherwise = traceError "m"


    checkSignature :: Bool
    checkSignature = 
      let !groupInfoPk = getGroupInfoParams groupInfo GPK
      in verify mode groupInfoPk hashRedeemer signature


    checkMint :: Bool
    checkMint = case flattenValue $ V2.txInfoMint info of
        [(symbol,name,a)] -> ((unCurrencySymbol symbol) == policy) && (a == amount) && ((unTokenName  name) == assetName) 
        -- _ -> traceError "mtm"


    checkTx :: Bool
    checkTx = 
        let !receivedValue =  valuePaidTo' info (PubKeyHash toPkhPay) toPkhStk
            !mintValue = V2.txInfoMint info
            !symbol = (CurrencySymbol policy)
            !tokenName = TokenName assetName
        in (valueOf receivedValue symbol tokenName) == (valueOf mintValue symbol tokenName) 
    
    checkTtl :: Bool
    !checkTtl = 
      let !range = V2.txInfoValidRange info
          !ttlRange = to (Plutus.POSIXTime ttl)
      in ttlRange == range


{-# INLINABLE mkValidator #-}
mkValidator :: GroupAdminNFTCheckTokenInfo ->() -> MintCheckRedeemer  -> V2.ScriptContext -> Bool
mkValidator storeman _ redeemer ctx = 
  case redeemer of
    BurnMintCheckToken -> burnTokenCheck storeman ctx
    MintCheckRedeemer mintCheckProof -> mintSpendCheck storeman mintCheckProof ctx


typedValidator :: GroupAdminNFTCheckTokenInfo -> PV2.TypedValidator TreasuryType
typedValidator = PV2.mkTypedValidatorParam @TreasuryType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator


validator :: GroupAdminNFTCheckTokenInfo -> Validator
validator = PV2.validatorScript . typedValidator

script :: GroupAdminNFTCheckTokenInfo -> Plutus.Script
script = unValidatorScript . validator

-- authorityCheckScriptShortBs :: GroupAdminNFTCheckTokenInfo -> SBS.ShortByteString
-- authorityCheckScriptShortBs = SBS.toShort . LBS.toStrict $ serialise . script

-- mintCheckScript :: CurrencySymbol -> PlutusScript PlutusScriptV2
-- mintCheckScript = PlutusScriptSerialised . authorityCheckScriptShortBs

mintCheckScript :: GroupAdminNFTCheckTokenInfo ->  PlutusScript PlutusScriptV2
mintCheckScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

mintCheckScriptHash :: GroupAdminNFTCheckTokenInfo -> Plutus.ValidatorHash
mintCheckScriptHash = PV2.validatorHash .typedValidator

-- authorityCheckScriptHashStr :: GroupAdminNFTCheckTokenInfo -> BuiltinByteString
-- authorityCheckScriptHashStr = case PlutusTx.fromBuiltinData $ PlutusTx.toBuiltinData . mintCheckScriptHash of 
--   Just s -> s
--   Nothing -> ""

mintCheckAddress ::GroupAdminNFTCheckTokenInfo -> Ledger.Address
mintCheckAddress = PV2.validatorAddress . typedValidator
