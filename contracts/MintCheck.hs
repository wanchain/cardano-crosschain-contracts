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
  , MintCheckRedeemer (..)
  , MintCheckRedeemer
  , MintCheckParams (..)
  , MintCheckParams
  ) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Prelude hiding (($),(<>), (&&), (&&), (==),(||),(>=),(<=),(<),(-),not,length,filter,(>),(!!),map,head,reverse,any,elem,snd,mconcat,negate,all)

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


data MintCheckRedeemer = MintCheckRedeemer
  {
    to :: BuiltinByteString -- send to 
    , policy :: BuiltinByteString -- which token , zero indicated only transfer ada
    , assetName :: BuiltinByteString
    , amount :: Integer  -- token amount
    -- , adaAmount :: Integer -- addtional ada amount
    , txHash :: BuiltinByteString
    , index :: Integer
    , mode :: Integer
    , uniqueId :: BuiltinByteString
    -- , txType :: Integer
    , signature :: BuiltinByteString
  }deriving (Prelude.Eq, Show)


PlutusTx.unstableMakeIsData ''MintCheckRedeemer
-- PlutusTx.makeLift ''MintCheckRedeemer

data TreasuryNFTDatum = TreasuryNFTDatum
  {
    policy_id :: BuiltinByteString -- send to 
    , tokenName :: BuiltinByteString -- which token , zero indicated only transfer ada
  }deriving (Prelude.Eq, Show)
PlutusTx.unstableMakeIsData ''TreasuryNFTDatum

data TreasuryType
instance Scripts.ValidatorTypes TreasuryType where
    type instance DatumType TreasuryType = ()
    type instance RedeemerType TreasuryType = MintCheckRedeemer

data MintCheckParams
  = MintCheckParams
      { groupInfoCurrency :: CurrencySymbol
        , groupInfoTokenName :: TokenName
      } deriving stock (Generic)
        deriving anyclass (ToJSON, FromJSON)

PlutusTx.unstableMakeIsData ''MintCheckParams
PlutusTx.makeLift ''MintCheckParams


{-# INLINABLE packInteger #-}
-- | Pack an integer into a byte string with a leading
-- sign byte in little-endian order
packInteger :: Integer -> BuiltinByteString
packInteger k -- = if k < 0 then consByteString 1 (go (negate k) emptyByteString) else consByteString 0 (go k emptyByteString)
  | k == 0 = consByteString 0 emptyByteString
  | k < 0  = consByteString 0x80 (go (negate k) emptyByteString)
  | otherwise = go k emptyByteString
    where
      go n s
        | n == 0            = s
        | otherwise         = go (n `PlutusTx.Prelude.divide` 256) (consByteString (n `modulo` 256) s)

{-# INLINABLE findOwnInput' #-}
findOwnInput' :: V2.ScriptContext -> Maybe V2.TxInInfo
findOwnInput' V2.ScriptContext{V2.scriptContextTxInfo=V2.TxInfo{V2.txInfoInputs},
             V2.scriptContextPurpose=Spending txOutRef} = go txInfoInputs
    where
        go [] = Nothing
        go (i@V2.TxInInfo{V2.txInInfoOutRef} : rest) = if txInInfoOutRef == txOutRef
                                                 then Just i
                                                 else go rest
findOwnInput' _ = Nothing

-- {-# INLINABLE ownAddress #-}
-- -- | Get the validator and datum hashes of the output that is curently being validated
-- ownAddress :: V2.ScriptContext -> Address
-- ownAddress (findOwnInput' -> Just V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutAddress=addr}}) = addr
-- ownAddress _ = traceError "Lg" -- "Can't get validator and datum hashes"

{-# INLINABLE getContinuingOutputs' #-}
-- | Get all the outputs that pay to the same script address we are currently spending from, if any.
getContinuingOutputs' :: V2.ScriptContext -> [V2.TxOut]
getContinuingOutputs' ctx | Just V2.TxInInfo{V2.txInInfoResolved=V2.TxOut{V2.txOutAddress}} <- V2.findOwnInput ctx = filter (f txOutAddress) (V2.txInfoOutputs $ V2.scriptContextTxInfo ctx)
    where
        f addr V2.TxOut{V2.txOutDatum=d, V2.txOutAddress=otherAddress, V2.txOutValue} = case d of
            OutputDatum datum -> addr == otherAddress
            _ -> False
getContinuingOutputs' _ = traceError "Lf" -- "Can't get any continuing outputs"


{-# INLINABLE mkValidator #-}
mkValidator :: MintCheckParams ->() -> MintCheckRedeemer -> V2.ScriptContext -> Bool
mkValidator (MintCheckParams groupInfoCurrency groupInfoTokenName) _ (MintCheckRedeemer to policy assetName amount txHash index mode uniqueId signature) ctx = 
  traceIfFalse "l" (V2.txSignedBy info  (PubKeyHash  (getGroupInfoParams groupInfo BalanceWorker))) && 
  traceIfFalse "hm" (hasUTxO ctx) && 
  traceIfFalse "hom" hasOwnOutput && 
  traceIfFalse "am" checkSignature &&  
  traceIfFalse "m" checkMint && 
  traceIfFalse "txm" checkTx
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


    hasOwnOutput :: Bool
    hasOwnOutput = 
      let !os = getContinuingOutputs' ctx
      in length os > 0


    hashRedeemer :: BuiltinByteString
    hashRedeemer = 
        let !tmp1 = appendByteString (appendByteString (appendByteString to policy) assetName) (packInteger amount) --(packInteger adaAmount)
            !tmp2 = appendByteString (appendByteString (appendByteString tmp1 txHash) (packInteger index)) (packInteger mode)
            !tmp3 = appendByteString tmp2 uniqueId
        in sha3_256 tmp3
    
    
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
        [(symbol,name,a)] -> ((unCurrencySymbol symbol) == policy)  && (a == amount) && ((unTokenName  name) == assetName) 
        -- _ -> traceError "mtm"


    checkTx :: Bool
    checkTx = 
        let !receivedValue =  V2.valuePaidTo info (PubKeyHash to)
            !mintValue = V2.txInfoMint info
            !symbol = (CurrencySymbol policy)
            !tokenName = TokenName assetName
        in (valueOf receivedValue symbol tokenName) == (valueOf mintValue symbol tokenName)


typedValidator :: MintCheckParams -> PV2.TypedValidator TreasuryType
typedValidator = PV2.mkTypedValidatorParam @TreasuryType
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = PV2.mkUntypedValidator


validator :: MintCheckParams -> Validator
validator = PV2.validatorScript . typedValidator

script :: MintCheckParams -> Plutus.Script
script = unValidatorScript . validator

-- authorityCheckScriptShortBs :: MintCheckParams -> SBS.ShortByteString
-- authorityCheckScriptShortBs = SBS.toShort . LBS.toStrict $ serialise . script

-- mintCheckScript :: CurrencySymbol -> PlutusScript PlutusScriptV2
-- mintCheckScript = PlutusScriptSerialised . authorityCheckScriptShortBs

mintCheckScript :: MintCheckParams ->  PlutusScript PlutusScriptV2
mintCheckScript p = PlutusScriptSerialised
  . SBS.toShort
  . LBS.toStrict
  $ serialise 
  (script p)

mintCheckScriptHash :: MintCheckParams -> Plutus.ValidatorHash
mintCheckScriptHash = PV2.validatorHash .typedValidator

-- authorityCheckScriptHashStr :: MintCheckParams -> BuiltinByteString
-- authorityCheckScriptHashStr = case PlutusTx.fromBuiltinData $ PlutusTx.toBuiltinData . mintCheckScriptHash of 
--   Just s -> s
--   Nothing -> ""

mintCheckAddress ::MintCheckParams -> Ledger.Address
mintCheckAddress = PV2.validatorAddress . typedValidator
