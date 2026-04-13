module Types where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)

type UserId = UUID
type Password = Text
type PasswordHash = Text
type Email = Text

data User = User
    { userId :: UserId
    , email :: Email
    , fullname :: Text
    , createdAt :: UTCTime
    , lastLoginAt :: Maybe UTCTime
    }
