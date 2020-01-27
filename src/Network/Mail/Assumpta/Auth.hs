
{-# LANGUAGE OverloadedStrings #-}

{- |

Authentication functions using 'MonadSmtp'.

Aims to eventually implement 
three widely-used SMTP authentication mechanisms:
Login, Plain, and Cram-MD5.

Currently, Login is implemented.

/TODO/: Add Cram-MD5, other auth methods.

-}

{-

Refs: see RFCs,
      https://networking.ringofsaturn.com/Protocols/smtpauth.php

SMTP authentication standards, courtesy of Wikipedia
(https://en.wikipedia.org/wiki/SMTP_Authentication):

-   PLAIN (Uses Base64 encoding)
-   LOGIN (Uses Base64 encoding)
-   GSSAPI (Generic Security Services Application Program Interface)
-   DIGEST-MD5 (Digest access authentication)
-   MD5
-   CRAM-MD5
-   OAUTH10A (OAuth 1.0a HMAC-SHA1 tokens as defined in RFC 5849)
-   OAUTHBEARER (OAuth 2.0 bearer tokens as defined in RFC 6750)

TODO: use text-conversions-0.3.0?
Has a dedicated Base64 newtype. 

TODO: add tests for the auth funcs.

-}

module Network.Mail.Assumpta.Auth
  (
    login
  , UserName
  , Password
  )
  where

import           Crypto.Hash (MD5)
import           Crypto.MAC.HMAC (hmac, HMAC)

import           Data.ByteArray.Encoding (convertToBase, Base(Base16))
import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as B64  (encode)
import qualified Data.ByteString.Char8 as B8

import Network.Mail.Assumpta.MonadSmtp

--  TODO - implement and SmtpAuth data type.


type UserName = ByteString
type Password = ByteString

-- newtype Password = Password ByteString
--instance Show Password where
--  show _ = "****"  

-- | Perform LOGIN authentication using the specified username
-- and password.
--
-- /TODO/: better reporting of errors when login fails.
-- Currently just throws an 'SMTPError' indicating that
-- @expectCode 235@ failed.
login :: MonadSmtp m => UserName -> Password -> m ()
login username password = do
  sendLine "AUTH LOGIN"
  expectCode 334
  let username' = B64.encode username
      password' = B64.encode password
  sendLine username'
  expectCode 334
  sendLine password'
  expectCode 235

-- | Not implemented yet -- TODO 
cramMd5 :: MonadSmtp m => UserName -> Password -> m ()
cramMd5 _username _password = do
  sendLine "XXXXX" 
  undefined

-- cramMD5 function from smtp-mail by
-- Jason Hickner and Matt Parsons 
cramMD5 :: String -> String -> String -> ByteString
cramMD5 challenge user pass =
    B64.encode $ B8.unwords [user', convertToBase Base16 hmac']
  where
    challenge' = toAscii challenge
    user'      = toAscii user
    pass'      = toAscii pass

    hmac' :: HMAC MD5
    hmac'      = hmac challenge' pass'

    toAscii :: String -> ByteString
    toAscii = BS.pack . map (toEnum.fromEnum)


