
{-# LANGUAGE OverloadedStrings #-}

{- |

Types for use with SMTP.

== Notes

Some notes on the underlying bits of the SMTP
protocol they model (see
<https://tools.ietf.org/html/rfc5321 RFC 5321> for
more details):

*   The protocol consists of /commands/ and /replies/.

*   /Replies/ can be single-line or multiline.
    Single lines start with @\<reply-code> \<SP>@;
    for multiple lines, every line except the last
    starts with @\<reply-code> \'-'@, and the last
    is a single line.

    The @\<SP>@ or @\'-'@ can be followed by optional text.
    Our 'ReplyLine' and 'Reply' store the code and the optional
    text (or an empty string if there was none), but
    not the @\<SP>@ or @\'-'@ separator.

*   A reply is a list of /one or more/ of these reply
    lines; but we just represent it as a list; it's an invariant
    that such a list should never be empty (and the
    parser promises never to return an empty list).

-}

{-
TODO: func converting codes into Eng. lang human
readable textual descriptions would be nice.

Consideration: It would be *nice*, when a parse fails, to
give knowledgeable callers the *exact*, pre-parse,
content that we received. But that sounds annoyingly fiddly.
And might never be used.

-}

module Network.Mail.Assumpta.Types
  where

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid -- needed for early Base

-- | Reply code from a server
type ReplyCode = Int

-- | Response from a serve
type Reply = [ReplyLine]

-- | One line of a reply from a server, consisting of a 'ReplyCode' and US-ASCII message.
data ReplyLine = ReplyLine {
      replyCode :: !ReplyCode
    , replyText  :: !ByteString
  }
  deriving (Show)

-- | SMTP commands. This type does not
-- include the \'@AUTH@' command, which
-- has a flexible form.
data SmtpCommand
    = HELO ByteString
    | EHLO ByteString
    | MAIL ByteString
    | RCPT ByteString
    | DATA
    | EXPN ByteString
    | VRFY ByteString
    | HELP (Maybe ByteString)
    | NOOP
    | RSET
    | QUIT
    | STARTTLS
    deriving (Show, Eq)

-- | Errors that can occur during SMTP operations.
--
-- These don't include connectivity and other IO errors
-- which might occur in the underlying transport
-- mechanism;
-- those should be handled elsewhere (if necessary).
--
-- The possible errors are that either (a) we couldn't
-- parse the server's response at all, or (b) we could,
-- but it wasn't what we expected.
data SmtpError

    -- | We received a response contrary
    --   to what we expected. The first
    --   field is a description of what we expected,
    --   the second of what we got.
  = UnexpectedResponse {
        expected :: String
      , received :: Reply
      }

  -- | We couldn't parse the server's
  --   response; the parser gave the
  --   error message contained in the 'ParseError'.
  | ParseError String
  deriving Show

-- | Dump an SMTP command to a bytestring, suitable for transmission to a
--   server. No CRLF is appended.
--
-- For commands like '@MAIL FROM:\<somesender\@somedomain>@':
--   this adds in the 'FROM:<' etc
--
-- >>> :set -XOverloadedStrings
-- >>> toByteString $ HELO "my-computer.org"
-- "HELO my-computer.org"
-- >>> toByteString $ MAIL "some-sender@my-org.com"
-- "MAIL FROM:<some-sender@my-org.com>"
-- >>> toByteString $ RCPT "delighted-recipient@dunder-mifflin.com"
-- "RCPT TO:<delighted-recipient@dunder-mifflin.com>"
toByteString :: SmtpCommand -> ByteString
toByteString command = case command of
    HELO bs         -> unwords ["HELO", bs]
    EHLO bs         -> unwords ["EHLO", bs]
    MAIL bs         -> "MAIL FROM:<" <> bs <> ">"
    RCPT bs         -> "RCPT TO:<" <> bs <> ">"
    EXPN bs         -> unwords ["EXPN", bs]
    VRFY bs         -> unwords ["VRFY", bs]
    HELP Nothing    -> "HELP"
    HELP (Just bs)  -> unwords ["HELP", bs]
    DATA            -> "DATA"
    NOOP            -> "NOOP"
    RSET            -> "RSET"
    QUIT            -> "QUIT"
    STARTTLS        -> "STARTTLS"
  where
    unwords = BS.unwords



