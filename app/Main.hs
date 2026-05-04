{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Data.Time.Clock (getCurrentTime)
import Database
import Languages
import Network.Wai.Middleware.Static
import Scotty
import Session (Session (expirationAliveTime))
import Session qualified as S
import Web.Scotty.Trans

-------------------------------------------------------------------------------
main :: IO ()
main = do
  db <- openSQLiteDatabase "db.sqlite"
  scotty
    AppParams
      { port = 3000
      , templatePath = "templates"
      , db = db
      , hashSalt = "some_salt"
      }
    $ do
      middleware $ staticPolicy (noDots >-> addBase "static")

      get "/" $
        withSession
          (redirect "/login")
          ( \S.Session{content = user, expirationAliveTime, expirationTime, ipAddress} -> do
              t <- liftIO getCurrentTime
              ginger "index.html" $ object ["user" .= user, "expires" .= expirationAliveTime, "expires2" .= expirationTime, "now" .= t, "ip" .= show ipAddress]
          )

      get "/favicon.ico" $ do
        setHeader "Content-Type" "image/x-icon"
        file "static/img/quick.ico"

      get "/:page" $ do
        page <- pathParam "page"
        ginger_ (page <> ".html")

      post "/api/login" $ do
        let
          ko err = ginger "login.html" $ object ["error_message" .= show (ShowAppError err)]
          ok _user = redirect "/"
        email <- formParam "email"
        password <- formParam "password"
        withLogin email password ko ok

      post "/api/logout" $ do
        let
          ko _err = redirect "/"
          ok = redirect "/"
        withLogout ko ok

      post "/api/signup" $ do
        let
          ko err = ginger "signup.html" $ object ["error_message" .= show (ShowAppError err)]
          ok = redirect "/"
        fullname <- formParam "fullname"
        email <- formParam "email"
        password <- formParam "password"
        confirmPassword <- formParam "confirm-password"
        withSignup
          fullname
          email
          password
          confirmPassword
          ko
          ok
