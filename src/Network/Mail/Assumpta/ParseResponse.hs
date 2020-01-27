
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

{- |

Parse server replies.

The general format of replies is described in 
<https://tools.ietf.org/html/rfc5321 RFC 5321>, at p. 49:

@
  The format for multiline replies requires that every line,
  except the last, begin with the reply code, followed
  immediately by a hyphen, "-" (also known as minus), followed by
  text.  The last line will begin with the reply code, followed
  immediately by \<SP>, optionally some text, and \<CRLF>.

     For example:
                         123-First line
                         123-Second line
                         123-234 text beginning with numbers
                         123 The last line
@

On p. 49, sec 4.2.2, "Reply Codes by Function Groups", the RFC lists the
various server responses that can be made.
(Also Wikipedia has a more pleasantly formatted version of the list at
<https://en.wikipedia.org/wiki/List_of_SMTP_server_return_codes>.)
-}

module Network.Mail.Assumpta.ParseResponse
  (
  -- * Fetching replies
  
  -- | 'getReply' is intended to be the main function
  -- used by other modules - it allows fetching and parsing replies
  -- from a server. The other functions are lower-level utility functions, and
  -- might be useful if you want to customize parsing.

    getReply

  -- * Low-level functions and types
  , Parser
  , textstring
  , code
  , reply
  , getReply_
  , parseFrom
  )

  where

import           Control.Monad.Except
import qualified Data.Attoparsec.ByteString.Char8 as Att
import           Data.Attoparsec.ByteString.Char8 ( decimal
                                                   , takeWhile1, string
                                                   , char, option
                                                   , space, many'
                                                   , parseWith, eitherResult
                                                   , (<?>) )
import qualified Data.ByteString as BS
import           Data.ByteString ( ByteString)
import           Data.Functor
import qualified Data.List as L
import           Data.Monoid -- needed for early versions of Base

import Network.Mail.Assumpta.Types

#if MIN_VERSION_mtl(2,2,2)
#else
liftEither :: MonadError e m => Either e a -> m a
liftEither = either throwError return
#endif

-- | <https://hackage.haskell.org/package/attoparsec Attoparsec> parser type.
type Parser = Att.Parser


-- | 
-- A string of length at least 1, which may contain printable US ASCII,
-- characters, space characters, and horizontal tabs.
--
-- See <https://tools.ietf.org/html/rfc5321 RFC 5321>, p. 47, "textstring".
--
-- The spec in the RFC is
--
-- @
-- textstring  = 1*(%d09 / %d32-126) ; HT, SP, Printable US-ASCII
-- @
--
-- >>> :set -XOverloadedStrings
-- >>> Att.parseOnly (textstring <* Att.endOfInput) "~" -- ASCII 126
-- Right "~"
-- >>> Att.parseOnly (textstring <* Att.endOfInput) "\x7f" -- ASCII 127
-- Left "printable ASCII text: Failed reading: takeWhile1"  
textstring :: Parser ByteString
textstring = (takeWhile1 is_printable) <?> "printable ASCII text"
  where
    -- 9 is horizontal tab, and [32, 126] is all printable US ASCII.
    is_printable c' = let c = fromEnum c' in c == 9 || (c >= 32 && c <= 126)
  -- textstring function courtesy of
  -- Alexander Vieth, smtp-mail-ng

-- | The @\<CRLF>@ sequence.
--
-- >>> :set -XOverloadedStrings
-- >>> Att.parseOnly (crlf <* Att.endOfInput) "\r\n"
-- Right ()
-- >>> Att.parseOnly (crlf <* Att.endOfInput) "\r\n."
-- Left "endOfInput"
crlf :: Parser ()
crlf = string "\r\n" $> ()

-- | Parser for a reply code.
--
-- The RFC states that an SMTP server SHOULD only send the codes 
-- listed in the spec, but we don't validate that here;
-- we accept any sequence of decimal digits, higher-level functions can
-- further validate it if desired.
--
-- >>> :set -XOverloadedStrings
-- >>> Att.parseOnly (code <* Att.endOfInput) "42"
-- Right 42
-- >>> Att.parseOnly (code <* Att.endOfInput) "042"
-- Right 42
code :: Parser ReplyCode
code = decimal

-- | One or more server reply lines, terminating with a
-- last line. The parser does not check that they all have the
-- same reply code.
reply :: Parser Reply
reply = (++) <$> many' continue 
             <*> (pure <$> lastLine)

-- | Last line of a (potentially multi-line) reply.
--
-- Code and <SP>, possibly a text string.
--
-- >>> :set -XOverloadedStrings
-- >>> let bs = "42 Answering your questions\r\n"
-- >>> Att.parseOnly (lastLine <* Att.endOfInput) bs
-- Right (ReplyLine {replyCode = 42, replyText = "Answering your questions"})
lastLine :: Parser ReplyLine
lastLine = ReplyLine  <$> code <* space
                      <*> option "" textstring <* crlf

-- | Continuation line of a multiline reply.
--
-- Code and '-' char, possibly a text string.
--
-- >>> :set -XOverloadedStrings
-- >>> let bs = "42-Answering your questions\r\n"
-- >>> Att.parseOnly (continue <* Att.endOfInput) bs
-- Right (ReplyLine {replyCode = 42, replyText = "Answering your questions"})
continue :: Parser ReplyLine
continue = ReplyLine  <$> code <* char '-'
                      <*> option "" textstring <* crlf

-- | @parseFrom x pull@
--
-- Given some action \'pull' which can be called to
-- supply the parser with more input, parse thing 'x'
-- and return a result.
parseFrom ::
  Monad m => Parser r -> m ByteString -> m (Either String r)
parseFrom x pull =
  eitherResult <$> parseWith pull x BS.empty

-- | @getReply_ pull@
--
-- Given some action \'pull' which can be called to
-- supply the parser with more input, attempt to
-- fetch input and parse it as a superficially
-- well-formed 'Reply' (multiple lines ending in a terminating
-- line).
-- We don't check that all reply lines have the
-- same reply code.
getReply_ :: Monad m => m ByteString -> m (Either String Reply)
getReply_ pull =
  eitherResult <$> parseWith pull reply BS.empty

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = all (== x) xs

toParseError :: Either String b -> Either SmtpError b
toParseError = either (Left . ParseError) Right

-- | @getReply pull@
--
-- Given some action \'pull' which can be called to
-- supply the parser with more input, attempt to fetch
-- content from the server and parse it as a 'Reply'.
-- On failure, 'ParseError' will be thrown, with
-- a message explaining the failure.
--
-- A 'ParseError' will also be thrown if the response
-- looks superficially like a reply, but has multiple
-- reply codes for different lines. (In other words,
-- a successful return value means the reply has at
-- least one line, and all lines have the same reply
-- code.)
--
-- The result returned is in the 'MonadError' monad,
-- so can be specialised by the caller to a 'Maybe',
-- 'Either', or some other 'MonadError' instance as desired.
getReply :: MonadError SmtpError m => m ByteString -> m Reply
getReply f =
  wellFormed =<< liftEither =<< toParseError <$> getReply_ f


-- | Check if reply is well-formed (has same reply code
-- for each line) else throw a 'ParseError'.
wellFormed ::
  MonadError SmtpError m => Reply -> m Reply
wellFormed replyLines =
  let codes = map replyCode replyLines
      mesg  = "Malformed reply contained multiple reply codes: "
                    <>  L.intercalate ", " (map show codes)
  in  if allSame codes
      then return replyLines
      else throwError (ParseError mesg)                   


