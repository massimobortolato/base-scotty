{-# LANGUAGE OverloadedStrings #-}

module Main where

import Database
import Network.Wai.Middleware.Static
import Scotty
import Web.Scotty.Trans

-------------------------------------------------------------------------------
main :: IO ()
main = do
  scotty
    ScottyParams
      { port = 3000
      , templatePath = "templates"
      , db = NoDatabase
      , isDebug = True
      }
    $ do
      middleware $ staticPolicy (noDots >-> addBase "static")

      get "/" $
        ginger_ "index.html"

-- get "/:page" $ do
--   page <- pathParam "page"
--   ctx <- readAllContexts -- readContext page
--   ginger ("/pages" </> page <> ".html") ctx

-- post "/api/login" $ do
--   email <- formParam "email"
--   password <- formParam "password"
--   withLogin
--     email
--     password
--     (ginger "/pages/login.html" $ object ["error_message" .= "Username o password errati"])
--     (redirect "/")
