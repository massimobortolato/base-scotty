module Types where

import Data.Aeson (ToJSON)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

type UserId = UUID
type Password = Text
newtype PasswordHash = PasswordHash Text
type Email = Text
type Fullname = Text

data User = User
    { userId :: UserId
    , email :: Email
    , fullname :: Fullname
    , createdAt :: UTCTime
    , lastLoginAt :: Maybe UTCTime
    }
    deriving (Show, Generic)

instance ToJSON User
