{-# LANGUAGE OverloadedStrings #-}

module Utils where

import Crypto.Error (CryptoFailable (CryptoPassed))
import Crypto.KDF.Argon2 (Options (iterations))
import Crypto.KDF.Argon2 qualified as Argon2
import Data.Aeson (Result (Error, Success), Value, fromJSON)
import Data.Base64.Types qualified as B64
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as B64
import Data.Text (Text)
import Data.Text qualified as T (pack)
import Language.Ginger qualified as G
import System.Random (StdGen)
import Text.Regex.TDFA ((=~))

---------------------------------------------------------------------------------------------------
type HashSalt = ByteString

---------------------------------------------------------------------------------------------------
hashPassword :: HashSalt -> ByteString -> Maybe ByteString
hashPassword salt pwd =
  case Argon2.hash opts pwd salt sz of
    CryptoPassed hash -> Just $ B64.extractBase64 $ B64.encodeBase64' hash
    _ -> Nothing
 where
  opts =
    Argon2.Options
      { iterations = 5
      , memory = 7168
      , parallelism = 1
      , variant = Argon2.Argon2id
      , version = Argon2.Version13
      }
  sz = 64

---------------------------------------------------------------------------------------------------
ginger :: StdGen -> FilePath -> FilePath -> Value -> IO (Either String Text)
ginger rnd templatePath filePath value =
  case fromJSON value of
    Error err -> pure $ Left err
    Success maps ->
      G.ginger
        (G.fileLoader templatePath)
        G.defPOptions
        G.DialectGinger2
        rnd
        G.htmlEncoder
        (T.pack filePath)
        maps
        >>= \case
          Left err -> pure $ Left $ show err
          Right (G.Encoded page) -> pure $ Right page

---------------------------------------------------------------------------------------------------
isValidEmail :: Text -> Bool
isValidEmail =
  (=~ regex)
 where
  regex = "^[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\\.[a-zA-Z0-9-.]+$" :: Text
