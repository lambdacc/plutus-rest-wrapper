{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Cardano.Address
import Cardano.Api
import Control.Monad.Except
import Control.Monad.Reader
import Data.Aeson
import Data.Aeson.Types
import Data.Attoparsec.ByteString
import Data.ByteString (ByteString)
import Data.Either.Unwrap
import Data.List
import Data.Maybe
import Data.Maybe ( fromMaybe )
import Data.String.Conversions
import Data.Text
import Data.Text.IO as TIO hiding (putStrLn)
import Data.Time.Calendar
import GHC.Generics
import Ledger (POSIXTime(..), PaymentPubKeyHash)
import Ledger.Tx.CardanoAPI
import Lucid
import Network.HTTP.Media ((//), (/:))
import Network.HTTP.Media ((//), (/:))
import Network.Wai
import Network.Wai.Handler.Warp
import Plutus.Rest.Utils (unsafeReadAddress, unsafePaymentPubKeyHash)
import Plutus.V1.Ledger.Address (toValidatorHash, scriptHashAddress)
import Prelude ()
import Prelude.Compat
import qualified Data.ByteString.Lazy  as LBS
import Servant
import Servant.Types.SourceT (source)
import System.Directory
import System.Environment              (getArgs)
import Text.Blaze
import Text.Blaze.Html.Renderer.Utf8


type AttrServerAPI = "pkh" :> Capture "a" String :> Get '[JSON] PlutusPkh

data PlutusPkh = PlutusPkh {pkh :: PaymentPubKeyHash}
                    deriving (Eq, Show, Generic, ToJSON)

attributeServer :: Server AttrServerAPI
attributeServer = pkh 
            where

              pkh :: String -> Handler PlutusPkh
              pkh a =  return $ PlutusPkh (mkPaymentPubKeyHash a)

mkPaymentPubKeyHash :: String -> PaymentPubKeyHash
mkPaymentPubKeyHash addr = unsafePaymentPubKeyHash $ unsafeReadAddress addr

attrServerAPI :: Proxy AttrServerAPI
attrServerAPI = Proxy

app :: Application
app = serve attrServerAPI attributeServer

main :: IO ()
main = do run 9033 app