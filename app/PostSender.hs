{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE OverloadedStrings #-}

module PostSender
    ( SendingPostError(..)
    , tweet
    , refresh
    ) where

import App
import Control.Lens
import Control.Monad.Catch
import Control.Monad.IO.Class                     (MonadIO, liftIO)
import Data.Aeson
import Data.Aeson.Lens
import Data.Time
import Data.Text                  qualified as T
import Data.Text.Encoding         qualified as T
import Data.Twitter                         as P
import Network.HTTP.Req


data SendingPostError
    = Unauthorized
    | TooManyRequests (Maybe UTCTime)
    | Other T.Text
    deriving (Show)

instance Exception SendingPostError

tweet :: (MonadIO m, HasTwitterToken m, MonadLogger m) => Post -> m (Either SendingPostError ())
tweet post = do
    token <- askTwitterToken
    logInfo "tweeting !"
    -- logInfo $ "text: " <> P.text post
    -- handle
    --     (\(e :: HttpException) ->
    --         case isStatusCodeException e of
    --             Nothing -> throwM e
    --             Just res ->
    --                 let scode = responseStatusCode res
    --                 in
    --                     if scode == 401 then
    --                         return $ Left Unauthorized
    --                     else
    --                         throwM e
    --     )
    runReq defaultHttpConfig { httpConfigCheckResponse = \_ _ _ -> Nothing } $ do
        res :: JsonResponse Value <- req POST
                (https "api.x.com" /: "2" /: "tweets")
                (ReqBodyJson post)
                jsonResponse
                (oAuth2Bearer $ T.encodeUtf8 $ accessToken token)
        let scode = responseStatusCode res
        if 200 <= scode && scode < 300 then
            return $ Right ()
        else if scode == 401 then
            return $ Left Unauthorized
        else if scode == 429 then do
            let reset = responseHeader res "x-rate-limit-reset"
            reset' <- liftIO $ traverse (parseTimeM True defaultTimeLocale "%s" . T.unpack . T.decodeUtf8) reset
            return $ Left $ TooManyRequests reset'
        else
            return $ Left $ Other $ T.decodeUtf8 $ responseStatusMessage res

refresh :: (MonadIO m) => Credential -> Token -> m (Maybe Token)
refresh cred (Token { refreshToken }) = do
    runReq defaultHttpConfig $ do
        res <- req POST
                   (https "api.x.com" /: "2" /: "oauth2" /: "token")
                   (ReqBodyUrlEnc params)
                   jsonResponse
                   (basicAuth (T.encodeUtf8 $ clientId cred) (T.encodeUtf8 $ clientSecret cred))
        let val = responseBody res :: Value
            accessToken'  = val ^? key "access_token"  . _String
            refreshToken' = val ^? key "refresh_token" . _String
            expires'      = fromIntegral . (`div` 2) <$> val ^? key "expires_in" . _Integer
        return $ Token <$> accessToken' <*> refreshToken' <*> pure expires'
  where
    params = "refresh_token" =: refreshToken
          <> "grant_type"    =: ("refresh_token" :: T.Text)
