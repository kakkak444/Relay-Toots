{-  Copyright © 2025 kakkak444.

    This Source Code Form is subject to the terms of the Mozilla Public
    License, v. 2.0. If a copy of the MPL was not distributed with this
    file, You can obtain one at https://mozilla.org/MPL/2.0/.
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

module Data.Event
    ( Account(..)
    , Event(..)
    , Toot(..)
    , TootTag(..)
    ) where

import Data.Aeson
import Data.Text      qualified as T
import Data.Time
import Data.Vector    qualified as V
import Deriving.Aeson
import Network.URI


data Event = Event
    { eventEvent     :: T.Text
    , eventCreatedAt :: ZonedTime
    , eventObject    :: Toot
    }
    deriving stock Generic
    deriving (FromJSON, ToJSON)
        via CustomJSON
           '[ FieldLabelModifier '[StripPrefix "event", CamelToSnake]
            ]
            Event

data Toot = Toot
    { tootId        :: T.Text
    , tootCreatedAt :: ZonedTime
    , tootUrl       :: Maybe URI
    , tootAccount   :: Account
    , tootContent   :: T.Text
    , tootTags      :: V.Vector TootTag
    }
    deriving stock Generic
    deriving (FromJSON, ToJSON)
        via CustomJSON
           '[ OmitNothingFields
            , FieldLabelModifier '[StripPrefix "toot", CamelToSnake]
            ]
            Toot

data TootTag = TootTag
    { tagName :: T.Text
    , tagUrl  :: URI
    }
    deriving stock Generic
    deriving (FromJSON, ToJSON)
        via CustomJSON
           '[ FieldLabelModifier '[StripPrefix "tag", CamelToSnake]
            ]
            TootTag

data Account = Account
    { acctUsername :: T.Text
    }
    deriving stock Generic
    deriving (FromJSON, ToJSON)
        via CustomJSON
           '[ FieldLabelModifier '[StripPrefix "acct", CamelToSnake]
            ]
            Account
