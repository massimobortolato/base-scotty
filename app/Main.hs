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
        redirect "/login"

      get "/favicon.ico" $ do
        setHeader "Content-Type" "image/x-icon"
        file "static/img/quick.ico"

      get "/:page" $ do
        page <- pathParam "page"
        ginger_ (page <> ".html")
