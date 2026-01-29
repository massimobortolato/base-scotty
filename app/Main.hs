{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Scotty
import Web.Scotty.Trans

-------------------------------------------------------------------------------
main :: IO ()
main =
  scotty
    ScottyParams
      { port = 3000
      , templatePath = "templates"
      }
    $ do
      get "/" $
        let
          obj =
            object
              [ "user"
                  .= object
                    [ "name" .= "massimo"
                    , "age" .= 44
                    ]
              ]
         in
          ginger "index.html" obj

      get "/simple" $
        ginger_ "simple.html"
