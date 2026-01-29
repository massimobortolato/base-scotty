module Scotty (
  ScottyParams (..),
  AppError (..),
  ActionM,
  ScottyM,
  ginger,
  ginger_,
  scotty,
)
where

import Control.Exception (Exception)
import Control.Monad.IO.Unlift (MonadIO, MonadUnliftIO)
import Control.Monad.Reader (
  MonadReader,
  ReaderT (..),
  asks,
 )
import Data.Aeson (Result (Error, Success), Value, fromJSON, object)
import Data.Text (pack)
import Data.Text.Lazy (fromStrict)
import Language.Ginger qualified as Ginger
import System.Random (StdGen, newStdGen)
import Web.Scotty.Trans (ActionT, ScottyT, html, scottyT, throw)

-------------------------------------------------------------------------------
data ScottyParams = ScottyParams
  { port :: Int
  , templatePath :: FilePath
  }

-------------------------------------------------------------------------------
newtype AppData = AppData
  { gingerFunc :: FilePath -> Value -> ActionM ()
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
  f <- asks gingerFunc
  f filePath value

-------------------------------------------------------------------------------
ginger_ :: FilePath -> ActionM ()
ginger_ filePath = ginger filePath $ object []

-------------------------------------------------------------------------------
scotty :: ScottyParams -> ScottyM () -> IO ()
scotty ScottyParams{port, templatePath} app = do
  rnd <- newStdGen
  scottyT port (runInIO rnd) app
 where
  runInIO :: StdGen -> AppM a -> IO a
  runInIO rnd = flip runReaderT AppData{gingerFunc = gingerFunc rnd} . runAppM
  gingerFunc :: StdGen -> FilePath -> Value -> ActionM ()
  gingerFunc rnd filePath value =
    case fromJSON value of
      Error err -> throw $ AppValueError err
      Success maps ->
        Ginger.ginger
          (Ginger.fileLoader templatePath)
          Ginger.defPOptions
          Ginger.DialectJinja2
          rnd
          Ginger.htmlEncoder
          (pack filePath)
          maps
          >>= \case
            Left err -> throw $ AppTemplateError (show err)
            Right (Ginger.Encoded page) -> html $ fromStrict page
