{-# LANGUAGE OverloadedStrings #-}

module Database (
  Database,
  openSQLiteDatabase,
  createUser,
  getUser,
)
where

import Control.Exception (SomeException (SomeException), catch)
import Control.Monad (mzero)
import Data.Pool
import Data.Time.Clock
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.SQLite.Simple qualified as SQLite
import Types

---------------------------------------------------------------------------------------------------
newtype Database = SQliteDatabase (Pool SQLite.Connection)

---------------------------------------------------------------------------------------------------
openSQLiteDatabase :: FilePath -> IO Database
openSQLiteDatabase fp = do
  let
    cfg =
      defaultPoolConfig
        (SQLite.open fp)
        SQLite.close
        60
        10
  pool <- newPool cfg
  createDb pool
  pure $ SQliteDatabase pool
 where
  createDb pool =
    withResource pool $ \conn -> do
      SQLite.execute_
        conn
        "CREATE TABLE IF NOT EXISTS users (id TEXT PRIMARY KEY, email TEXT UNIQUE, fullname TEXT, password_hash TEXT, created_at TIMESTAMP, last_login_at TIMESTAMP)"

---------------------------------------------------------------------------------------------------
createUser :: Database -> Email -> Fullname -> PasswordHash -> IO (Maybe User)
createUser (SQliteDatabase pool) email fullname (PasswordHash passwordHash) = do
  userId <- nextRandom
  t <- getCurrentTime
  withResource pool $ \conn -> do
    catch
      ( do
          SQLite.execute
            conn
            "INSERT INTO users (id, email, fullname, password_hash, created_at) VALUES (?, ?, ?, ?, ?)"
            (UUID.toText userId, email, fullname, passwordHash, t)
          pure $ Just User{userId = userId, email = email, fullname = fullname, createdAt = t, lastLoginAt = Nothing}
      )
      ( \(SomeException _) ->
          pure Nothing
      )

---------------------------------------------------------------------------------------------------
getUser :: Database -> Email -> PasswordHash -> IO (Maybe User)
getUser (SQliteDatabase pool) email (PasswordHash passwordHash) = do
  withResource pool $ \conn -> do
    results <-
      SQLite.query
        conn
        "SELECT * FROM users WHERE email = ? AND password_hash = ?"
        (email, passwordHash)
    case results of
      [SQLiteUser user] -> pure $ Just user
      _ -> pure Nothing

------------------------------------------------------------------------------------
newtype SQLiteUser = SQLiteUser User
instance SQLite.FromRow SQLiteUser where
  fromRow = do
    userId <- SQLite.field
    email <- SQLite.field
    fullname <- SQLite.field
    _passwordHash <- PasswordHash <$> SQLite.field
    createdAt <- SQLite.field
    lastLoginAt <- SQLite.field
    case UUID.fromText userId of
      Nothing -> mzero
      Just uid -> pure $ SQLiteUser User{userId = uid, email, fullname, createdAt, lastLoginAt}