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

module AttributeServer where

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
import Plutus.Rest.Utils (unsafeReadAddress, unsafePaymentPubKeyHash, addrToPaymentPubKeyHash)
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
import Sample.Contracts.Escrow as E
import Sample.Runtime


type AttrServerAPI = "pkh" :> Capture "a" String :> Get '[JSON] PlutusPkh
                :<|> "script-address" :> ReqBody '[JSON] RequestBody :> Post '[JSON] ScriptAddr
                :<|> "script-cbor" :> ReqBody '[JSON] RequestBody :> Post '[JSON] ScriptCBOR

data PlutusPkh = PlutusPkh {pkh :: PaymentPubKeyHash}
                    deriving (Eq, Show, Generic, ToJSON)

data ScriptAddr = ScriptAddr {escrowAddress :: Text}
                    deriving (Eq, Show, Generic, ToJSON)

data ScriptCBOR = ScriptCBOR
                      { typeKey     :: !String
                      , description :: !String
                      , cborHex     :: !ByteString
                      } deriving (Eq, Show, Generic)

instance FromJSON ScriptCBOR where
   parseJSON (Object v) = ScriptCBOR <$> v .: "type" <*> v .: "description" <*> v .: "cborHex"

instance ToJSON ScriptCBOR where
  toJSON (ScriptCBOR { typeKey = typeKey, description = description, cborHex = cborHex }) =
    object [ "type" .= typeKey
           , "description"  .= description
           , "cborHex"  .= cborHex
           ]

data RequestBody = RequestBody
  { reqBuyer :: String
  , reqSeller :: String
  , reqAmount :: Integer
  , reqFinaliseTime :: Integer
  , reqEndTime :: Integer
  , reqNetwork :: String
  }
  deriving (Generic, ToJSON, FromJSON, Show)

attributeServer :: Server AttrServerAPI
attributeServer = pkh
             :<|> scrAddresses
             :<|> scrCbor
              where

              pkh :: String -> Handler PlutusPkh
              pkh a =  return $ PlutusPkh (addrToPaymentPubKeyHash a)

              scrAddresses :: RequestBody -> Handler ScriptAddr
              scrAddresses b = do
                              let ep  = toEscrowParams b
                                  nw = case reqNetwork b of
                                          "testnet" -> (Testnet $ NetworkMagic 1)
                                          _ -> Mainnet
                                  shelleyAddr = toCardanoAddressInEra (nw) $ E.scrAddress ep
                              return $ ScriptAddr (serialiseAddress $ fromRight shelleyAddr)

              scrCbor :: RequestBody -> Handler ScriptCBOR
              scrCbor b = do
                              let file = "/tmp/scriptCbor.plutus"
                              liftIO $ writeEscrowValidator (toEscrowParams b) file
                              content <- liftIO $ LBS.readFile file
                              return $ fromJust (decode content)

toEscrowParams :: RequestBody -> EscrowParams
toEscrowParams b = E.EscrowParams
                    { buyer = addrToPaymentPubKeyHash (reqBuyer b)
                    , seller = addrToPaymentPubKeyHash (reqSeller b)
                    , lovelaceAmt = reqAmount b
                    , finaliseTime = POSIXTime $ reqFinaliseTime b
                    , endTime = POSIXTime $ reqEndTime b
                    }

attrServerAPI :: Proxy AttrServerAPI
attrServerAPI = Proxy

app :: Application
app = serve attrServerAPI attributeServer

defaultMain :: IO ()
defaultMain = do run 9033 app