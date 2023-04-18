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

-- import Data.Aeson (FromJSON, ToJSON)
-- import GHC.Generics (Generic)
-- import Ledger (PubKey, Signature)
-- import Plutus.Script.Utils.V2.Typed.Scripts.Validators (ValidatorTypes)
-- import Plutus.V1.Ledger.Value (AssetClass)
-- import Plutus.V2.Ledger.Api (PubKeyHash, TxOutRef)
-- import PlutusTx qualified
-- import PlutusTx.Prelude
-- -- import Prelude qualified
-- import Plutus.V2.Ledger.Api
-- import Plutus.V1.Ledger.Api (TxOutRef, Value, DatumHash)
-- -- import qualified PlutusTx
-- import qualified Prelude as Haskell

-- module CustomContexts
--   ( CustomScriptContext (..),
--     TxInfo (..),
--     ScriptPurpose (..),
--     TxOut (..),
--   )
-- where



-- -- | A transaction output, consisting of a target address, a value, and optionally a datum hash.
-- -- data TxOut = TxOut
-- --   { txOutAddress :: BuiltinData,
-- --     txOutValue :: Value,
-- --     txOutDatumHash :: Maybe DatumHash
-- --   }
-- --   deriving stock (Generic, Haskell.Eq)

-- instance Eq TxOut where
--   {-# INLINEABLE (==) #-}
--   l == r =
--     txOutAddress l == txOutAddress r
--       && txOutValue l == txOutValue r
--       && txOutDatumHash l == txOutDatumHash r

-- -- | Purpose of the script that is currently running
-- -- data ScriptPurpose
-- --   = Minting BuiltinData
-- --   | Spending !TxOutRef
-- --   | Rewarding BuiltinData
-- --   | Certifying BuiltinData
-- --   deriving stock (Generic, Haskell.Show, Haskell.Eq)

-- instance Eq ScriptPurpose where
--   {-# INLINEABLE (==) #-}
--   Minting cs == Minting cs' = cs == cs'
--   Spending ref == Spending ref' = ref == ref'
--   Rewarding sc == Rewarding sc' = sc == sc'
--   Certifying cert == Certifying cert' = cert == cert'
--   _ == _ = False

-- -- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
-- data TxInfo = TxInfo
--   { -- | Transaction inputs
--     txInfoInputs :: BuiltinData,
--     -- | Transaction outputs
--     txInfoOutputs :: [TxOut],
--     -- | The fee paid by this transaction.
--     txInfoFee :: BuiltinData,
--     -- | The 'Value' minted by this transaction.
--     txInfoMint :: BuiltinData,
--     -- | Digests of certificates included in this transaction
--     txInfoDCert :: BuiltinData,
--     -- | Withdrawals
--     txInfoWdrl :: BuiltinData,
--     -- | The valid range for the transaction.
--     txInfoValidRange :: BuiltinData,
--     -- | Signatures provided with the transaction, attested that they all signed the tx
--     txInfoSignatories :: BuiltinData,
--     txInfoData :: BuiltinData,
--     -- | Hash of the pending transaction (excluding witnesses)
--     txInfoId :: BuiltinData
--   }
--   deriving stock (Generic, Haskell.Eq)

-- instance Eq TxInfo where
--   {-# INLINEABLE (==) #-}
--   TxInfo i o f m c w r s d tid == TxInfo i' o' f' m' c' w' r' s' d' tid' =
--     i == i' && o == o' && f == f' && m == m' && c == c' && w == w' && r == r' && s == s' && d == d' && tid == tid'

-- data CustomScriptContext = CustomScriptContext {scriptContextTxInfo :: TxInfo, scriptContextPurpose :: ScriptPurpose}
--   deriving stock (Generic, Haskell.Eq)

-- instance Eq CustomScriptContext where
--   {-# INLINEABLE (==) #-}
--   CustomScriptContext info purpose == CustomScriptContext info' purpose' = info == info' && purpose == purpose'

-- PlutusTx.makeLift ''TxOut
-- PlutusTx.makeIsDataIndexed ''TxOut [('TxOut, 0)]

-- PlutusTx.makeLift ''TxInfo
-- PlutusTx.makeIsDataIndexed ''TxInfo [('TxInfo, 0)]

-- PlutusTx.makeLift ''CustomScriptContext
-- PlutusTx.makeIsDataIndexed ''CustomScriptContext [('CustomScriptContext, 0)]

-- PlutusTx.makeLift ''ScriptPurpose
-- PlutusTx.makeIsDataIndexed
--   ''ScriptPurpose
--   [ ('Minting, 0),
--     ('Spending, 1),
--     ('Rewarding, 2),
--     ('Certifying, 3)
--   ]