{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module Scotty (
  ScottyParams (..),
  AppError (..),
  ActionM,
  ScottyM,
  SignupError (..),
  ginger,
  ginger_,
  scotty,
  withSignup,
  withLogin,
)
where

import Control.Exception (Exception)
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Data.Aeson (Result (Error, Success), Value, fromJSON, object, (.=))
import Data.Text (Text, pack)
import Data.Text qualified as T (length, null)
import Data.Text.Lazy (fromStrict)
import Types
import Database
import Language.Ginger qualified as Ginger
import System.Random (StdGen, newStdGen)
import Text.Regex.TDFA ((=~))
import Web.Scotty.Trans (ActionT, ScottyT, html, scottyT, throw)
import Web.Scotty.Session

-------------------------------------------------------------------------------
data SignupError
  = PasswordMismatch
  | WeakPassword
  | InvalidEmail
  | InvalidFullname
  | UserAlreadyExists
  deriving (Show)

-------------------------------------------------------------------------------
data SessionPayload = SessionPayload
  { userId :: UserId
  , csrfToken :: Text
  }

-------------------------------------------------------------------------------
data ScottyParams = ScottyParams
  { port :: Int
  , templatePath :: FilePath
  , db :: Database
  , isDebug :: Bool
  }

-------------------------------------------------------------------------------
data AppData = AppData
  { gingerFunc :: FilePath -> Value -> ActionM ()
  , jar :: SessionJar SessionPayload
  , db :: Database
  }

-------------------------------------------------------------------------------
data AppError
  = AppValueError String
  | AppTemplateError String
  deriving (Show)
instance Exception AppError

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
  AppData{gingerFunc} <- ask
  gingerFunc filePath value

-------------------------------------------------------------------------------
ginger_ :: FilePath -> ActionM ()
ginger_ filePath = ginger filePath $ object []

-------------------------------------------------------------------------------
scotty :: ScottyParams -> ScottyM () -> IO ()
scotty ScottyParams{port, templatePath, db, isDebug} app = do
  rnd <- newStdGen
  jar' <- createSessionJar
  scottyT port (runInIO rnd db jar') app
 where
  runInIO :: StdGen -> Database -> SessionJar SessionPayload -> AppM a -> IO a
  runInIO rnd db' jar' AppM{runAppM} = runReaderT runAppM AppData{gingerFunc = gingerFunc rnd, db = db', jar = jar'}
  gingerFunc :: StdGen -> FilePath -> Value -> ActionM ()
  gingerFunc rnd filePath value =
    case fromJSON value of
      Error err ->
        if isDebug
          then gingerFunc rnd "ginger_error.html" (object ["message" .= ("Error parsing template data: " ++ err)])
          else throw $ AppValueError err
      Success maps ->
        Ginger.ginger
          (Ginger.fileLoader templatePath)
          Ginger.defPOptions
          Ginger.DialectGinger2
          rnd
          Ginger.htmlEncoder
          (pack filePath)
          maps
          >>= \case
            Left err ->
              if isDebug
                then gingerFunc rnd "ginger_error.html" (object ["message" .= ("Error rendering template: " ++ show err)])
                else throw $ AppTemplateError (show err)
            Right (Ginger.Encoded page) -> html $ fromStrict page

-------------------------------------------------------------------------------
withSignup :: Text -> Email -> Password -> Password -> (SignupError -> ActionM ()) -> ActionM () -> ActionM ()
withSignup fullname email password confirm_password onError onSuccess
  | password /= confirm_password = onError PasswordMismatch
  | T.length password < 8 = onError WeakPassword
  | not isValidEmail = onError InvalidEmail
  | T.null fullname = onError InvalidFullname
  | otherwise = do
      AppData{db} <- ask
      userId <- liftIO $ createUser db User{userId = undefined, email = email, fullname = fullname, createdAt = undefined, lastLoginAt = Nothing} password
      case userId of
        Nothing -> onError UserAlreadyExists
        Just _ -> onSuccess
 where
  isValidEmail :: Bool
  isValidEmail =
    email =~ regex
   where
    regex = "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$" :: Text

-------------------------------------------------------------------------------
withLogin :: Email -> Password -> ActionM () -> ActionM () -> ActionM ()
withLogin email password onError onSuccess = do
  AppData{db} <- ask
  userId <- liftIO $ isUser db email password
  case userId of
    Nothing -> onError
    Just _ -> onSuccess
