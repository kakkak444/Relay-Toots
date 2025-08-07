{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module TootReceiver
    ( TootReceiverAPI

    , tootReceiver
    , tootReceiverApi
    ) where

import App
import Control.Monad
import Control.Monad.IO.Class                     (liftIO)
import Crypto.Hash
import Crypto.MAC.HMAC
import Data.Aeson
import Data.Event
import Data.Bifunctor
import Data.ByteString                            (ByteString)
import Data.ByteString.Lazy       qualified as LB
import Data.Text                  qualified as T
import Network.HTTP.Media                         ((//))
import Servant


data WithRawJson
instance Accept WithRawJson where
    contentType Proxy = "application" // "json"
instance (FromJSON a) => MimeUnrender WithRawJson (LB.ByteString, a) where
    mimeUnrender Proxy bs = second (bs, ) $ eitherDecode bs


type TootReceiverAPI = Header "X-Hub-Signature" T.Text :> ReqBody '[WithRawJson] (LB.ByteString, Event) :> PostNoContent

tootReceiverApi :: Proxy TootReceiverAPI
tootReceiverApi = Proxy


calculateSignature :: ByteString -> LB.ByteString -> Digest SHA256
calculateSignature key = hmacGetDigest . hmacLazy key


tootReceiver :: (Event -> AppT IO ()) -> Maybe T.Text -> (LB.ByteString, Event) -> AppT Handler NoContent
tootReceiver _cont Nothing          _ = return NoContent
tootReceiver  cont (Just signature) (raw, event) = do
    secretKey <- askSecretKey
    let calcSig = T.pack $ "sha256=" <> show (calculateSignature secretKey raw)
    unless (calcSig /= signature) $ do
        hoistApp liftIO $ cont event
    return NoContent
