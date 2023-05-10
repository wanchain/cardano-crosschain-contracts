{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fobject-code #-}

module CrossChain.Types where

import Prelude hiding((&&),(==),($),(!!),(<),(>),(/=),negate,filter,map,snd,mconcat,any,not)
import GHC.Generics (Generic)
-- import Builtins qualified as Builtins
import Data.ByteString qualified as ByteString
import Plutus.V1.Ledger.Bytes (LedgerBytes (LedgerBytes),fromBytes,getLedgerBytes)
-- import Plutus.V1.Ledger.Value (TokenName (..), CurrencySymbol)
import Plutus.V2.Ledger.Api (TokenName (..), CurrencySymbol,TxOutRef, Value, DatumHash, TxId (..)
    , TxOut(..)
    -- , TxInfo
    -- , ScriptPurpose
    , TxInInfo
    , TxOutRef(..)
    , OutputDatum (..),Datum (..),Address(..),ScriptHash (..)
    ,fromBuiltinData,getDatum,ValidatorHash (..),Credential (..)
    , StakingCredential (..), PubKeyHash (..)
    )
-- import Plutus.V2.Ledger.Tx as V2
import PlutusTx --(CompiledCode, Lift, UnsafeFromData (unsafeFromBuiltinData), applyCode, liftCode,BuiltinData,makeLift,makeIsDataIndexed)
import PlutusTx.Prelude 
-- import Prelude (check)
import Plutus.Script.Utils.Typed (UntypedValidator)
import Plutus.V2.Ledger.Contexts as V2
import Plutus.V1.Ledger.Value (valueOf,currencySymbol,tokenName,symbols,flattenValue,assetClass,assetClassValueOf)
import Ledger.Ada  as Ada




{-# INLINABLE mkUntypedValidator' #-}
mkUntypedValidator'
    :: forall d r
    . (UnsafeFromData d, UnsafeFromData r)
    => (d -> r -> BuiltinData -> Bool)
    -> UntypedValidator
-- We can use unsafeFromBuiltinData here as we would fail immediately anyway if parsing failed
mkUntypedValidator' f d r p =
    check $ f (unsafeFromBuiltinData d) (unsafeFromBuiltinData r) (unsafeFromBuiltinData p)



-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
data TxOut' = TxOut' {
    txOutAddress'         :: BuiltinData,
    txOutValue'           :: Value,
    txOutDatum'           :: BuiltinData,
    txOutReferenceScript' :: BuiltinData
    }
    deriving stock (Generic, Prelude.Eq)

data TxInInfo' = TxInInfo'
    { txInInfoOutRef'   :: BuiltinData
    , txInInfoResolved' :: TxOut'
    } deriving (Generic, Prelude.Eq)


data TxInfo' = TxInfo'
  { txInfoInputs'          :: [TxInInfo'] -- ^ Transaction inputs
    , txInfoReferenceInputs' :: BuiltinData -- ^ Transaction reference inputs
    , txInfoOutputs'        :: BuiltinData -- ^ Transaction outputs
    , txInfoFee'            :: BuiltinData -- ^ The fee paid by this transaction.
    , txInfoMint'           :: BuiltinData -- ^ The 'Value' minted by this transaction.
    , txInfoDCert'          :: BuiltinData -- ^ Digests of certificates included in this transaction
    , txInfoWdrl'           :: BuiltinData -- ^ Withdrawals
    , txInfoValidRange'     :: BuiltinData -- ^ The valid range for the transaction.
    , txInfoSignatories'    :: BuiltinData -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoRedeemers'      :: BuiltinData
    , txInfoData'           :: BuiltinData
    , txInfoId'             :: BuiltinData
    -- ^ Hash of the pending transaction (excluding witnesses)
    } deriving (Generic, Prelude.Eq)

data StoremanScriptContext = StoremanScriptContext {scriptContextTxInfo' :: TxInfo', scriptContextPurpose :: BuiltinData}
  deriving (Generic, Prelude.Eq)

-- instance Eq StoremanScriptContext where
--     {-# INLINABLE (==) #-}
--     StoremanScriptContext info purpose == StoremanScriptContext info' purpose' = info == info' && purpose == purpose'

data ParamType = Version | Admin | GPK | BalanceWorker | TreasuryCheckVH | OracleWorker | MintCheckVH  | StkPKh
data GroupInfoParams
  = GroupInfoParams
      { params :: [BuiltinByteString]
      } deriving (Prelude.Eq, Prelude.Show)


data CheckTokenInfoParam
  = CheckTokenInfoParam
      { 
        -- groupInfoNFTCurrency :: CurrencySymbol
        -- , groupInfoNFTName :: TokenName
        checkTokenSymbol :: CurrencySymbol
        , checkTokenName :: TokenName
      } deriving stock (Generic)
        -- deriving anyclass (ToJSON, FromJSON)

PlutusTx.makeLift ''CheckTokenInfoParam
PlutusTx.makeIsDataIndexed ''CheckTokenInfoParam [('CheckTokenInfoParam, 0)]

PlutusTx.makeLift ''TxOut'
PlutusTx.makeIsDataIndexed ''TxOut' [('TxOut', 0)]

PlutusTx.makeLift ''TxInInfo'
PlutusTx.makeIsDataIndexed ''TxInInfo' [('TxInInfo', 0)]

PlutusTx.makeLift ''TxInfo'
PlutusTx.makeIsDataIndexed ''TxInfo' [('TxInfo', 0)]

PlutusTx.makeLift ''StoremanScriptContext
PlutusTx.makeIsDataIndexed ''StoremanScriptContext [('StoremanScriptContext, 0)]

PlutusTx.makeLift ''ParamType
PlutusTx.makeIsDataIndexed ''ParamType [('Version, 0),('Admin, 1),('GPK, 2),('BalanceWorker, 3),('TreasuryCheckVH, 4),('OracleWorker,5),('MintCheckVH, 6),('StkPKh, 7)]

PlutusTx.makeLift ''GroupInfoParams
PlutusTx.makeIsDataIndexed ''GroupInfoParams [('GroupInfoParams, 0)]


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
    StkPKh -> params !! 7

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

{-# INLINABLE getGroupInfo #-}
getGroupInfo :: V2.TxInfo -> CurrencySymbol -> TokenName -> GroupInfoParams
getGroupInfo V2.TxInfo{V2.txInfoReferenceInputs} groupInfoCurrency groupInfoTokenName = 
  case filter (isGroupInfoToken) $ map (V2.txInInfoResolved) txInfoReferenceInputs of
    [o] ->case o of 
      (V2.TxOut _ _ outputDatum _) -> case outputDatum of
          (OutputDatum datum ) -> case (fromBuiltinData $ getDatum datum) of
            Just groupInfo -> groupInfo
  where
    isGroupInfoToken :: V2.TxOut -> Bool
    isGroupInfoToken (V2.TxOut (Address credential _) txOutValue _ _) = (assetClassValueOf txOutValue ( assetClass groupInfoCurrency groupInfoTokenName)) > 0

{-# INLINABLE getTotalAmountOfAssetInInput #-}
getTotalAmountOfAssetInInput :: V2.ScriptContext -> CurrencySymbol -> TokenName -> Integer
getTotalAmountOfAssetInInput ctx checkTokenSymbol checkTokenName = 
      let !totoalOutValue = V2.valueSpent (V2.scriptContextTxInfo ctx)
          !totalOutAmount = valueOf totoalOutValue checkTokenSymbol checkTokenName
      in totalOutAmount


{-# INLINABLE isSingleAsset #-}
isSingleAsset :: Value -> CurrencySymbol -> TokenName -> Bool
isSingleAsset v cs tk = not $ any (\(cs',tk',_) -> cs' /= cs && cs' /= Ada.adaSymbol && tk' /= tk && tk' /= Ada.adaToken) $ flattenValue v

{-# INLINABLE getAmountOfCheckTokeninOwnOutput #-}
getAmountOfCheckTokeninOwnOutput :: V2.ScriptContext  -> CurrencySymbol -> TokenName-> BuiltinByteString -> Integer
getAmountOfCheckTokeninOwnOutput ctx checkTokenSymbol checkTokenName stk = 
      let !lockedValue = valueLockedBy' (V2.scriptContextTxInfo ctx) (V2.ownHash ctx) stk
          !lockedAmount = valueOf lockedValue checkTokenSymbol checkTokenName
      in lockedAmount
      
{-# INLINABLE getAmountOfCheckTokenInOutput #-}
getAmountOfCheckTokenInOutput :: V2.ScriptContext  -> CurrencySymbol -> TokenName-> Integer
getAmountOfCheckTokenInOutput ctx checkTokenSymbol checkTokenName = 
      let !outputValue = V2.valueProduced (V2.scriptContextTxInfo ctx)
          !lockedAmount = valueOf outputValue checkTokenSymbol checkTokenName
      in 
        if (isSingleAsset outputValue checkTokenSymbol checkTokenName) then lockedAmount
        else traceError "mtk"

{-# INLINABLE scriptOutputsAt' #-}
scriptOutputsAt' :: ValidatorHash -> BuiltinByteString -> V2.TxInfo -> [(Datum, Value)]
scriptOutputsAt' h stk p =
    let flt V2.TxOut{V2.txOutDatum=d, V2.txOutAddress=Address (ScriptCredential s) stk', V2.txOutValue} | s == h && (stakeCredentialToBytes stk') == stk = case d of
          OutputDatum datum -> Just (datum, txOutValue)
          _ -> Nothing
        flt _ = Nothing
    in mapMaybe flt (V2.txInfoOutputs p)

{-# INLINABLE valueLockedBy' #-}
valueLockedBy' :: V2.TxInfo -> ValidatorHash -> BuiltinByteString -> Value
valueLockedBy' ptx h stk =
    let outputs = map snd (scriptOutputsAt' h stk ptx)
    in mconcat outputs

{-# INLINABLE pubKeyOutputsAt' #-}
-- | Get the values paid to a public key address by a pending transaction.
pubKeyOutputsAt' :: PubKeyHash -> TxInfo ->  BuiltinByteString -> [Value]
pubKeyOutputsAt' pk p stk =
    let flt TxOut{txOutAddress = Address (PubKeyCredential pk') stk', txOutValue} | pk == pk' &&  stk == (stakeCredentialToBytes stk')= Just txOutValue
        flt _                             = Nothing
    in mapMaybe flt (txInfoOutputs p)

{-# INLINABLE valuePaidTo' #-}
-- | Get the total value paid to a public key address by a pending transaction.
valuePaidTo' :: TxInfo -> PubKeyHash -> BuiltinByteString -> Value
valuePaidTo' ptx pkh stk = mconcat (pubKeyOutputsAt' pkh ptx stk)

{-# INLINEABLE stakeCredentialToBytes #-}
stakeCredentialToBytes :: Maybe StakingCredential -> BuiltinByteString
stakeCredentialToBytes stk =  case stk of
  Just stkh -> case stkh of
    StakingHash c -> case c of
      PubKeyCredential pkh ->getPubKeyHash pkh
      ScriptCredential (ValidatorHash s) -> s
  Nothing -> emptyByteString