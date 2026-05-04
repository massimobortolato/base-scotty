{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

-- {-# LANGUAGE OverloadedStrings #-}

module Scotty (
  AppParams (..),
  AppError (..),
  ActionM,
  ScottyM,
  AppSession,
  ginger,
  ginger_,
  scotty,
  withSignup,
  withLogin,
  withLogout,
  withSession,
)
where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception)
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson (Value, object)
import Data.ByteString (ByteString)
import Data.Text qualified as T (length, null)
import Data.Text.Encoding qualified as T (decodeUtf8, encodeUtf8)
import Data.Text.Lazy (fromStrict)
import Database qualified as DB
import Session (SessionConfig (sessionAliveTime))
import Session qualified as S
import System.Random (StdGen, newStdGen)
import Types
import Utils qualified as U
import Web.Scotty.Trans (ActionT, ScottyT, html, scottyT, throw)

-------------------------------------------------------------------------------
type HashSalt = ByteString

type AppSessionJar = S.SessionJar User
type AppSession = S.Session User

-------------------------------------------------------------------------------
data AppError
  = PasswordMismatch
  | WeakPassword
  | InvalidEmail
  | InvalidFullname
  | UserAlreadyExists
  | HashError
  | InvalidCredentials
  | NoSession
  | GingerError String
  deriving (Show, Eq)

instance Exception AppError

-------------------------------------------------------------------------------
data AppParams = AppParams
  { port :: Int
  , templatePath :: FilePath
  , db :: DB.Database
  , hashSalt :: HashSalt
  }

-------------------------------------------------------------------------------
data AppData = AppData
  { ginger :: FilePath -> Value -> ActionM ()
  , hasher :: Password -> Maybe PasswordHash
  , jar :: AppSessionJar
  , db :: DB.Database
  }

-------------------------------------------------------------------------------
newtype AppM a = AppM
  { runAppM :: ReaderT AppData IO a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AppData, MonadUnliftIO)

-------------------------------------------------------------------------------
type ActionM = ActionT AppM
type ScottyM = ScottyT AppM

-------------------------------------------------------------------------------
ginger :: FilePath -> Value -> ActionM ()
ginger filePath value = do
  AppData{ginger = g} <- ask
  g filePath value

-------------------------------------------------------------------------------
ginger_ :: FilePath -> ActionM ()
ginger_ filePath = ginger filePath $ object []

-------------------------------------------------------------------------------
scotty :: AppParams -> ScottyM () -> IO ()
scotty AppParams{port, templatePath, db, hashSalt} app = do
  rnd <- newStdGen
  jar <- S.createSessionJar S.SessionConfig{sessionExpireTime = S.Days 1, sessionAliveTime = S.Hours 1}
  scottyT port (runInIO rnd jar) app
 where
  runInIO :: StdGen -> AppSessionJar -> AppM a -> IO a
  runInIO rnd jar AppM{runAppM} =
    runReaderT
      runAppM
      AppData
        { ginger = g rnd templatePath
        , hasher = fmap (PasswordHash . T.decodeUtf8) . U.hashPassword hashSalt . T.encodeUtf8
        , db = db
        , jar = jar
        }
  g rnd tpath fpath v = do
    res <- liftIO $ U.ginger rnd tpath fpath v
    case res of
      Left err -> throw $ GingerError err
      Right page -> html $ fromStrict page

-------------------------------------------------------------------------------
withSignup :: Fullname -> Email -> Password -> Password -> (AppError -> ActionM ()) -> ActionM () -> ActionM ()
withSignup fullname email password confirm_password onError onSuccess
  | password /= confirm_password = onError PasswordMismatch
  | T.length password < 8 = onError WeakPassword
  | not (U.isValidEmail email) = onError InvalidEmail
  | T.null fullname = onError InvalidFullname
  | otherwise = do
      AppData{db, jar, hasher} <- ask
      let maybeHash = hasher password
      case maybeHash of
        Nothing -> onError HashError
        Just passwordHash -> do
          maybeUser <- liftIO $ DB.createUser db email fullname passwordHash
          case maybeUser of
            Nothing -> liftIO (threadDelay 3_000_000) >> onError UserAlreadyExists
            Just user ->
              S.createSession jar user
                >> onSuccess

-------------------------------------------------------------------------------
withLogin :: Email -> Password -> (AppError -> ActionM ()) -> (User -> ActionM ()) -> ActionM ()
withLogin email password onError onSuccess = do
  AppData{db, hasher} <- ask
  let maybeHash = hasher password
  case maybeHash of
    Nothing -> onError HashError
    Just passwordHash -> do
      maybeUser <- liftIO $ DB.getUser db email passwordHash
      case maybeUser of
        Just user@User{userId} -> do
          AppData{jar} <- ask
          S.createSession jar user
            >> liftIO (DB.loginUser db userId)
            >> onSuccess user
        Nothing -> liftIO (threadDelay 3_000_000) >> onError InvalidCredentials

-------------------------------------------------------------------------------
withLogout :: (AppError -> ActionM ()) -> ActionM () -> ActionM ()
withLogout onError onSuccess = do
  AppData{jar} <- ask
  eitherSession <- S.getSession jar
  case eitherSession of
    Left _ -> onError NoSession
    Right S.Session{token} ->
      S.deleteSession jar token
        >> onSuccess

-------------------------------------------------------------------------------
withSession :: ActionM () -> (AppSession -> ActionM ()) -> ActionM ()
withSession onNoSession onSession = do
  AppData{jar} <- ask
  eitherSession <- S.getSession jar
  case eitherSession of
    Left _ -> onNoSession
    Right session -> onSession session
