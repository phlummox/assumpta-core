

{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{- |

A monad for sending SMTP commands and checking
for expected responses.

== #permissiblecharacters# Permissible characters

This module accepts 'ByteString's as parameters, but it is
the responsibility of the caller to ensure that the bytestrings
meet the requirements of the appropriate RFC; we do not
validate them.

In general, <https://tools.ietf.org/html/rfc5321 RFC 5321>
commands and replies must be composed of composed of characters from
the ASCII character set (with the possible exception of content
supplied after a \'DATA' command); see sec 2.4 of the RFC.
That is, they must be '7-bit clean'.

RFC 5321 notes that although various SMTP extensions (such as "8BITMIME",
<https://tools.ietf.org/html/rfc1652 RFC 1652>) may relax this restriction for
the content body, content header fields are always encoded using US-ASCII.
See also <https://tools.ietf.org/html/rfc3030 RFC 3030>, "SMTP Service Extensions",
for details of suppling DATA in non-ASCII format.

Note also that unless increased using some SMTP extension,
RFC 5321 imposes maximum sizes on the length of (\<CRLF>-terminated)
lines sent to the server (see sec. 4.5.3, "Sizes and Timeouts").
Again, we don't enforce these requirements, it's up to the caller
to check that they're satisfied.

== Constructing email messages

This package does not provide facilities for constructing email
messages, but only sending them via SMTP.
See the 
<https://hackage.haskell.org/package/mime-mail mime-mail>
package to construct and properly render email messages.

-}

module Network.Mail.Assumpta.MonadSmtp
    (
      -- * SMTP monad
      MonadSmtp(..)
      -- * SMTP commands
    , helo
    , ehlo
    , mailFrom
    , rcptTo
    , data_
    , noop
    , quit
    , rset
    , startTLS
    , expn
    , vrfy
    , help
      -- * Server responses
    , expect
    , expectGreeting
      -- * Low-level MonadSmtp operations 
    , sendLine
    , command
      -- * Send an email message
    , sendRawMail
      -- * Types
    , SmtpCommand(..)
    , Reply
    , ReplyLine(..)
    , ReplyCode
    , SmtpError(..)
    , ByteString
      -- * Utility functions
    , crlf
    ) 
  where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Trans.Maybe as Maybe

-- for transformer instances
-- __TODO__: finish off doing these
import Control.Monad.Trans.Identity
import Control.Monad.Trans.RWS.Lazy       as LazyRWS
import Control.Monad.Trans.RWS.Strict     as StrictRWS
import Control.Monad.Trans.State.Lazy     as LazyState
import Control.Monad.Trans.State.Strict   as StrictState
import Control.Monad.Trans.Writer.Lazy    as LazyWriter
import Control.Monad.Trans.Writer.Strict  as StrictWriter

import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString ( ByteString )
import           Data.Monoid -- needed for early versions of Base
import           Data.String

import Network.Mail.Assumpta.Types as Types

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

-- | Monad for sending SMTP commands and checking
-- for expected responses.
--
class Monad m => MonadSmtp m where
  -- | Send some bytes.
  send :: ByteString -> m ()

  -- | Attempt to read a response from the server,
  -- parsing it as a 'Reply'.
  getReply :: m Reply

  -- | Attempt to read and parse a server response, indicating that we
  -- expect it to be the given 'ReplyCode'.
  --
  -- In some 'MonadSmtp' instances, failure of the expectation will result
  -- in an exception being thrown.
  -- If you are writing an instance of @MonadSmtp m@
  -- where @MonadError SmtpError m@ holds, we can supply a default
  -- implementation for you.
  expectCode :: ReplyCode -> m ()

  default expectCode :: (MonadError SmtpError m) => ReplyCode -> m ()
  expectCode expectedCode = void $ 
      expect (== expectedCode) (show expectedCode)

  -- | Upgrade from plain STMP to SMTPS using default TLS
  -- settings
  tlsUpgrade :: m ()
  -- TODO: might want to throw an error if someone tries to
  -- call tlsUpgrade on a channel that is already secure.

instance MonadSmtp m => MonadSmtp (IdentityT m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance MonadSmtp m => MonadSmtp (MaybeT m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance MonadSmtp m => MonadSmtp (ReaderT r m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance (Monoid w, MonadSmtp m) => MonadSmtp (LazyRWS.RWST r w s m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance (Monoid w, MonadSmtp m) => MonadSmtp (StrictRWS.RWST r w s m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance (MonadSmtp m) => MonadSmtp (LazyState.StateT s m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance (MonadSmtp m) => MonadSmtp (StrictState.StateT s m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance (Monoid w, MonadSmtp m) => MonadSmtp (StrictWriter.WriterT w m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance (Monoid w, MonadSmtp m) => MonadSmtp (LazyWriter.WriterT w m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

instance (MonadSmtp m) => MonadSmtp (ExceptT e m) where
    send        = lift . send
    getReply    = lift getReply
    tlsUpgrade  = lift tlsUpgrade
    expectCode  = lift . expectCode

--instance MonadError e m => MonadError e (ReaderT r m) where
--    throwError = lift . throwError
--    catchError = Reader.liftCatch catchError

-- __TODO__: Monad {Reader, Writer, State}

-- | @expect pred expectDescrip@  
-- 
-- Fetch a reply, and validate that its reply code
-- meets predicate @pred@; on failure, an
-- 'UnexpectedResponse' is thrown into the 'MonadError'
-- monad. (So a caller can easily convert it to a
-- 'Maybe' or 'Either' or any other instance.)
--
-- Takes a human-readable
-- description of what was expected, which is
-- included in the exception.
--
-- Useful for implementing 'expectCode'.
expect ::
  (MonadSmtp m, MonadError SmtpError m) =>
      (ReplyCode -> Bool) -> String -> m Reply
expect pred expectDescrip =
    getReply >>= meetsPred
  where 
    meetsPred r =      
      if pred $ replyCode $ head r
        then return r
        else throwError $ UnexpectedResponse expectDescrip r

-- | Send some bytes, with a 'crlf' inserted at the end.
sendLine :: MonadSmtp m => ByteString -> m ()
sendLine bs = send $ bs <> crlf

-- | Send a command, without waiting for the reply.
command :: MonadSmtp m => SmtpCommand -> m ()
command cmd =
  sendLine (toByteString cmd)

unaryCommand :: MonadSmtp m => 
  (a -> SmtpCommand) -> a -> m ()
unaryCommand f =
  sendLine . toByteString . f

-- | Expect code 220, a "Service ready" message (or
-- "greeting").
--
-- Every client session should /start/ by waiting
-- for the server to send a "Service ready" message.
expectGreeting :: MonadSmtp m => m ()
expectGreeting =
  expectCode 220


-- | Convenience func. 
--
-- @helo myhostname@ will send '@HELO myhostname@', expect 250.
helo :: MonadSmtp m => ByteString -> m ()
helo bs =  
  unaryCommand HELO bs >>
    expectCode 250

-- | Convenience func. 
--
-- @ehlo myhostname@ will send '@EHLO myhostname@', expect 250.
ehlo :: MonadSmtp m => ByteString -> m ()
ehlo clientHostName = 
        unaryCommand EHLO clientHostName >>
          expectCode 250

-- | Convenience func. 
--
-- @mailFrom sender@ will send '@MAIL FROM:\<sender\>@', expect 250.
mailFrom :: MonadSmtp m => ByteString -> m ()
mailFrom sender =
  unaryCommand MAIL sender >>
    expectCode 250

-- | Convenience func. 
--
-- @rcptTo recipient@ will send '@RCPT TO:\<recipient\>@', expect 250.
rcptTo :: MonadSmtp m => ByteString -> m ()
rcptTo recipient =
  unaryCommand RCPT recipient >>
    expectCode 250


-- | convenience func. Send a \'DATA' command, expect 354,
-- send bytestring content (which should be terminated by
-- the sequence \<CRLF.CRLF>),
-- expect 354.
--
-- See <https://tools.ietf.org/html/rfc5321 RFC 5321> for
-- details of the 'DATA' command.
--
-- Prerequisites:
--
-- *   "The mail data may contain any of the 128 ASCII character codes,
--     although experience has indicated that use of control characters
--     other than SP, HT, CR, and LF may cause problems and SHOULD be
--     avoided when possible." [RFC 5321, p. 35]
--
--     We don't check that the bytestring being sent is indeed 7-bit
--     clean; that's up to the caller.
--
-- *  Any periods at the start of a line SHOULD be escaped.
--    (See RFC 5321, p. 61, \"Transparency".) It is up to the caller
--    to ensure this has been done.
--
-- *  The content passed should end in \'@\<CRLF.CRLF>@' (i.e.,
--    a @\<CRLF>@, then a full stop on a line by itself,
--    then @\<CRLF>@. We don't check that this is the case.
data_ :: MonadSmtp m => ByteString -> m ()
data_ bs = do 
  command DATA
  expectCode 354
  send bs
  expectCode 250

-- | Convenience func. Send NOOP,
--  expect 250.
--
--  See 
--  <https://tools.ietf.org/html/rfc5321 RFC 5321>,
--  p. 39, sec 4.1.1.9 ("NOOP (NOOP)")
noop :: MonadSmtp m => m ()
noop = do
  command NOOP
  expectCode 250

-- | Convenience func. Send QUIT,
--  expect 221.
--
--  See <https://tools.ietf.org/html/rfc5321 RFC 5321>,
--  p. 39, sec 4.1.1.10 ("QUIT (QUIT)").
quit :: MonadSmtp m => m ()
quit = do
  command QUIT
  expectCode 221


-- | Convenience func. Send RSET (used to abort
-- transaction), expect 250.
--
--  See <https://tools.ietf.org/html/rfc5321 RFC 5321>,
--  p. 37, sec 4.1.1.5 ("RESET (RSET)").
rset :: MonadSmtp m => m ()
rset = do
  command RSET
  expectCode 250

-- | Try to get TLS going on an SMTP connection.
--
-- After this, you should send an EHLO.
--
-- RFC reference: __???__
startTLS :: MonadSmtp m => m ()
startTLS = do
  command STARTTLS
  expectCode 220
  tlsUpgrade

-- | Convenience func. 
--
-- @help myhostname@ will send \'@HELP myhostname@' and 
-- attempt to parse a 'Reply'.
help :: MonadSmtp m => Maybe ByteString -> m Reply
help bs =  
  unaryCommand HELP bs >>
    getReply

-- | Convenience func. 
--
-- @expn recipient@ will send '@EXPN recipient@' and 
-- attempt to parse a 'Reply'. The 'EXPN' command
-- asks the server to verify that the recipient is
-- a mailing list, and return the members of the
-- list. Many servers restrict access to this 
-- command.
expn :: MonadSmtp m => ByteString -> m Reply
expn recipient = 
  unaryCommand EXPN recipient >>
    getReply

-- | Convenience func. 
--
-- @vrfy recipient@ will send '@VRFY recipient@' and 
-- attempt to parse a 'Reply'. The 'VRFY' command
-- asks the server to confirm that the argument
-- identifies a user or mailbox.
-- Many servers restrict access to this 
-- command
vrfy :: MonadSmtp m => ByteString -> m Reply
vrfy recipient = 
  unaryCommand VRFY recipient >>
    getReply

-- | @sendRawMail sender recipients message@
--
-- convenience func. Expects a raw 'ByteString'
-- that can be sent after a data command.
--
-- sends MAIL FROM, RCPT TO commands as appropriate,
-- expecting 250 in each case.
-- Then sends data, likewise expecting 250.
--
-- We don't alter the content of @message@, expect insofar
-- as specified by RFC, p. 36, namely:
-- If the body content passed does not end in @\<CRLF>@, a
-- client must either reject the message as invalid or 
-- add @\<CRLF>@ to the end;
-- we do the latter. (We are not permitted to alter the content
-- in any other case.)
--
-- We then append the \'@\<.CRLF>@' used to terminate the data
-- (this is not considered part of the message).
--
-- Other than that, the same requirements apply as for
-- the 'data_' function.
sendRawMail ::
  (MonadSmtp m, Foldable t) =>
      ByteString -> t ByteString -> ByteString -> m ()
sendRawMail sender recipients message = do
  mailFrom sender
  expectCode 250
  mapM_ rcptTo recipients

  let messageEnd =
        if crlf `BSC.isSuffixOf` message
        then "." <> crlf
        else crlf <> "." <> crlf
  data_ $ message <> messageEnd


-- | A @"\\r\\n"@ sequence, indicated @\<CRLF>@ in the RFC,
-- used to terminate all lines sent.
crlf :: Data.String.IsString p => p
crlf = "\r\n"


    
