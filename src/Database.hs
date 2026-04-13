{-# LANGUAGE OverloadedStrings #-}

module Database where

import Data.UUID qualified as UUID
import Types

data Database
    = NoDatabase

---------------------------------------------------------------------------------------------------
createUser :: Database -> User -> PasswordHash -> IO (Maybe UserId)
createUser _ _ _ = pure $ Just UUID.nil

---------------------------------------------------------------------------------------------------
isUser :: Database -> Email -> PasswordHash -> IO (Maybe UserId)
isUser _ _ _ = pure $ Just UUID.nil

