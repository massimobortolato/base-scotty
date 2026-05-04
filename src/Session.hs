{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Session (
    Session (..),
    SessionToken,
    SessionCsrfToken,
    SessionJar,
    SessionStatus,
    SessionTime (..),
    SessionConfig (..),
    createSessionJar,
    createSession,
    getSession,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Base64.Types qualified as B64
import Data.ByteString as BS (ByteString, take, takeEnd)
import Data.ByteString.Base64 qualified as B64
import Data.Functor ((<&>))
import Data.HashMap.Strict qualified as HM
import Data.Text qualified as T
import Data.Text.Encoding qualified as T (encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import Network.Socket (SockAddr)
import Network.Wai (Request (remoteHost))
import Web.Scotty.Cookie qualified as CK
import Web.Scotty.Trans (ActionT, request)

-- | Type alias for session identifiers.
type SessionToken = BS.ByteString

type SessionCsrfToken = BS.ByteString
type IpAddress = SockAddr

-- | Status of a session lookup.
data SessionStatus = SessionNotFound | SessionExpired
    deriving (Show, Eq)

data SessionTime
    = Seconds Int
    | Minutes Int
    | Hours Int
    | Days Int

-- | Represents a session containing an ID, expiration time, and content.
data Session a = Session
    { token :: SessionToken
    -- ^ Unique identifier for the session.
    , csrfToken :: SessionCsrfToken
    -- ^ CSRF token for the session.
    , expirationTime :: UTCTime
    , expirationAliveTime :: UTCTime
    , ipAddress :: IpAddress
    , content :: a
    -- ^ Content stored in the session.
    }
    deriving (Eq, Show)

-- | Type for session storage, a transactional variable containing a map of session IDs to sessions.
data SessionJar a = SessionJar
    { config :: SessionConfig
    , storage :: TVar (HM.HashMap SessionToken (Session a))
    }

data SessionConfig = SessionConfig
    { sessionExpireTime :: SessionTime
    , sessionAliveTime :: SessionTime
    }

-- | Creates a new session jar and starts a background thread to maintain it.
createSessionJar :: SessionConfig -> IO (SessionJar a)
createSessionJar config = do
    storage <- newTVarIO HM.empty
    let sessionJar = SessionJar{config = config, storage = storage}
    _ <- forkIO $ maintainSessions sessionJar
    pure sessionJar

-- | Continuously removes expired sessions from the session jar.
maintainSessions :: SessionJar a -> IO ()
maintainSessions sessionJar =
    forever $ do
        now <- getCurrentTime
        atomically $ modifyTVar (storage sessionJar) $ \m -> HM.filter (stillValid now) m
        threadDelay 10_000_000

-- | Adds or overwrites a new session to the session jar.
addSession' :: SessionJar a -> Session a -> IO ()
addSession' sessionJar sess =
    atomically $ modifyTVar (storage sessionJar) $ \m -> HM.insert (token sess) sess m

-- | Retrieves a session by its ID from the session jar.
getSession' :: (MonadIO m) => SessionJar a -> SessionToken -> ActionT m (Either SessionStatus (Session a))
getSession' sessionJar token' = do
    s <- liftIO $ readTVarIO (storage sessionJar)
    ip <- request <&> remoteHost
    case HM.lookup token' s of
        Nothing -> pure $ Left SessionNotFound
        Just sess -> do
            now <- liftIO getCurrentTime
            if not (stillValid now sess)
                -- \|| (ipAddress sess /= ip)
                then deleteSession sessionJar (token sess) >> pure (Left SessionExpired)
                else do
                    let sess' =
                            sess
                                { expirationAliveTime = addUTCTime (fromSessionTime $ sessionAliveTime (config sessionJar)) now
                                , ipAddress = ip
                                }
                    liftIO $ addSession' sessionJar sess'
                    pure $ Right sess'

-- | Deletes a session by its ID from the session jar.
deleteSession :: (MonadIO m) => SessionJar a -> SessionToken -> ActionT m ()
deleteSession sessionJar token =
    liftIO $
        atomically $
            modifyTVar (storage sessionJar) $
                HM.delete token

stillValid :: UTCTime -> Session a -> Bool
stillValid now Session{expirationTime, expirationAliveTime} =
    expirationTime > now && expirationAliveTime > now

{- | Retrieves the current user's session based on the "sess_id" cookie.
| pures `Left SessionStatus` if the session is expired or does not exist.
-}
getSession :: (MonadIO m) => SessionJar a -> ActionT m (Either SessionStatus (Session a))
getSession sessionJar = do
    CK.getCookie sessionCookieName >>= \case
        Nothing -> pure $ Left SessionNotFound
        Just sid -> lookupSession $ T.encodeUtf8 sid
  where
    lookupSession = getSession' sessionJar

-- | Creates a new session for a user, storing the content and setting a cookie.
createSession :: (MonadIO m) => SessionJar a -> a -> ActionT m (Session a)
createSession sessionJar content = do
    ip <- request <&> remoteHost
    sess@Session{token} <- liftIO $ createSession' sessionJar ip content
    CK.setCookie $
        CK.defaultSetCookie
            { CK.setCookieName = T.encodeUtf8 sessionCookieName
            , CK.setCookieValue = token
            , CK.setCookiePath = Just "/"
            , CK.setCookieHttpOnly = True
            , CK.setCookieSameSite = Just CK.sameSiteStrict
            }
    pure sess

{- | Creates a new session with a generated ID, sets its expiration,
| and adds it to the session jar.
-}
createSession' :: SessionJar a -> IpAddress -> a -> IO (Session a)
createSession' sessionJar ip content = do
    (token, csrf_token) <- newSessionTokens
    now <- getCurrentTime
    let sess =
            Session
                { token = token
                , csrfToken = csrf_token
                , expirationTime = addUTCTime (fromSessionTime (sessionExpireTime (config sessionJar))) now
                , expirationAliveTime = addUTCTime (fromSessionTime (sessionAliveTime (config sessionJar))) now
                , ipAddress = ip
                , content = content
                }
    liftIO $ addSession' sessionJar sess
    pure sess

{- | Generates new session and CSRF tokens.
| Returns a tuple of (session token, CSRF token).
-}
newSessionTokens :: IO (ByteString, ByteString)
newSessionTokens = do
    bytes <- getRandomBytes (session_id_sz + csrf_token_sz)
    let
        session_id = (B64.extractBase64 . B64.encodeBase64') (BS.take session_id_sz bytes)
        csrf_token = (B64.extractBase64 . B64.encodeBase64') (BS.takeEnd session_id_sz bytes)
    pure (session_id, csrf_token)
  where
    session_id_sz = 32
    csrf_token_sz = 32

fromSessionTime :: SessionTime -> NominalDiffTime
fromSessionTime = fromIntegral . fromSessionTime'
  where
    fromSessionTime' (Seconds n) = n
    fromSessionTime' (Minutes n) = 60 * n
    fromSessionTime' (Hours n) = 60 * 60 * n
    fromSessionTime' (Days n) = 24 * 60 * 60 * n

sessionCookieName :: T.Text
sessionCookieName = "sess_id"