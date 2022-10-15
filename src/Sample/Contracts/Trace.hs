{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module  Sample.Contracts.Trace where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras                   as Extras
import           Ledger
import           Plutus.Contract            as Contract
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (IO, String, show, div)
import           Wallet.Emulator.Wallet
import           Test.Tasty
import qualified Test.Tasty.HUnit                             as HUnit
import           Data.Monoid                                  (Last (..))
import           Plutus.Contract.Trace      as X
import           Plutus.V1.Ledger.Time      (POSIXTime (..))
import           Sample.Contracts.Escrow

testContractCollect :: IO ()
testContractCollect = runEmulatorTraceIO contractCollectTrace

testContractCancel :: IO ()
testContractCancel = runEmulatorTraceIO contractCancelTrace

buyerW, sellerW :: Wallet
buyerW = X.knownWallet 1
sellerW  = X.knownWallet 2

escrowParams :: Integer -> POSIXTime -> POSIXTime -> EscrowParams
escrowParams amt ftime etime = EscrowParams
  { buyer = mockWalletPaymentPubKeyHash buyerW
  , seller = mockWalletPaymentPubKeyHash sellerW
  , lovelaceAmt = amt
  , finaliseTime = ftime
  , endTime = etime
  }

contractCollectTrace  :: EmulatorTrace ()
contractCollectTrace = do
    h1 <- activateContractWallet buyerW endpoints
    h2 <- activateContractWallet sellerW endpoints

    let amt = 90000000
        escrow = escrowParams amt (POSIXTime 0) (POSIXTime 10000)
    callEndpoint @"lock" h1 escrow
    void $ Emulator.waitNSlots 11
    callEndpoint @"collect" h2 escrow
    void $ Emulator.waitNSlots 2

contractCancelTrace  :: EmulatorTrace ()
contractCancelTrace = do
    h1 <- activateContractWallet buyerW endpoints
    h2 <- activateContractWallet sellerW endpoints

    let amt = 90000000
        escrow = escrowParams amt (POSIXTime 0) (POSIXTime 10000)
    callEndpoint @"lock" h1 escrow
    void $ Emulator.waitNSlots 11
    callEndpoint @"collect" h2 escrow
    void $ Emulator.waitNSlots 2

