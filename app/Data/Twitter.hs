{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Twitter
    ( Credential(..)
    , Token(..)
    , Post

    , credentialToBase64
    , mkPost
    , postMaxChars
    , postUrlChars
    , text
    ) where

import Prelude                                hiding (truncate)

import Data.Aeson
import Data.ByteString        qualified as BS
import Data.ByteString.Base64 qualified as BS
import Data.Text              qualified as T
import Data.Text.Encoding     qualified as T
import Data.Time.Clock
import GHC.Generics


postMaxChars :: Int
postMaxChars = 140

postUrlChars :: Int
postUrlChars = 23

data Credential = Credential
    { clientId     :: T.Text
    , clientSecret :: T.Text
    } deriving (Eq)

data Token = Token
    { accessToken  :: T.Text
    , refreshToken :: T.Text
    , expiresIn    :: Maybe NominalDiffTime
    } deriving (Eq)

data Post = Post
    { text :: T.Text
    }
    deriving stock (Eq, Generic, Show)
    deriving (FromJSON, ToJSON)
         via Generically (Post)


mkPost :: T.Text -> Post
mkPost text = Post { text = text' }
  where
    text' =
        if T.length text <= postMaxChars then
            text
        else
            T.take (postMaxChars - 3) text <> "..."

credentialToBase64 :: Credential -> BS.ByteString
credentialToBase64 (Credential { clientId, clientSecret }) =
    let content = T.encodeUtf8 $ clientId <> ":" <> clientSecret
    in
        BS.encode content
