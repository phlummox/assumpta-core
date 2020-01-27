
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Network.Mail.Assumpta.MonadSmtpSpec
  (main, spec)
  where

import Test.Hspec
import Test.QuickCheck

import Data.String

import Network.Mail.Assumpta.MonadSmtp as S
import Network.Mail.Assumpta.Mock as Mock

import Control.Monad.Writer
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString (ByteString) 
import qualified Data.ByteString.Char8 as BS (intercalate)


sampleMesgLines :: IsString s => [s]
sampleMesgLines = [
    "Date: Tue, 21 Jan 2020 02:28:37 +0800"
  , "To: Someone"
  , "From: Someone else"
  , "Subject: test message"
  , ""
  , "This is a test mailing"
  ]

-- | given an intercalate function, produce a message
-- from sampleMesgLines which does NOT end in a crlf.
joinMesg :: (IsString s) => (s -> [s] -> s) -> s
joinMesg f =
  f crlf sampleMesgLines

-- | sequences of commands in the SMTP monad,
-- and the bytestrings we expect to see sent by them.
-- We *don't* validate that inputs are sensible --
-- assumpta-core trusts the user to only pass
-- data that is valid according to the relevant RFCs.
--
-- tuple is: (description, test seq, expected response)
testSequences :: [(String, MockSmtp (), BS.ByteString)]
testSequences = [
    ( "trivial session"
    , ehlo "myhost.mydomain" >>
      quit
    , "EHLO myhost.mydomain\r\nQUIT\r\n"
    )

  , ( "expecting code sends nothing"
    , expectCode 250
    , ""
    )
 
  -- for message content not ending in CRLF: we must add a CRLF
  -- (plus the terminating ".\r\n"). 
  , let bs = joinMesg BS.intercalate
    in
    ( "short message not ending in CRLF, sent via sendRawMail"
    , 
      sendRawMail "sender@s.com" ["recipient@r.com"] bs
    , "MAIL FROM:<sender@s.com>\r\nRCPT TO:<recipient@r.com>\r\nDATA\r\n"
      <> bs <> "\r\n.\r\n"
    )

  -- for message content that DOES end in CRLF, we can only
  -- add the terminating  ".\r\n".
  , let bs = joinMesg BS.intercalate <> crlf
    in
    ( "short message ending in CRLF, sent via sendRawMail"
    , 
      sendRawMail "sender@s.com" ["recipient@r.com"] bs
    , "MAIL FROM:<sender@s.com>\r\nRCPT TO:<recipient@r.com>\r\nDATA\r\n"
      <> bs <> ".\r\n"
    ) 

  -- ** no-param commands
  , nullaryCommand noop "NOOP"
  , nullaryCommand quit "QUIT"
  , nullaryCommand rset "RSET"
  , nullaryCommand startTLS "STARTTLS"
  ]

-- | e.g. @nullaryCommand quit "QUIT"@
-- specifies that the result of executing the command-sequence
-- quit
-- is just that we send the string "QUIT", terminated with crlf.
nullaryCommand :: MockSmtp () -> (forall s . IsString s => s) ->
                    (String, MockSmtp (), ByteString)
nullaryCommand a str =
  (str <> " command", a, str <> (crlf :: ByteString))

-- | check that a sequence of MockSmtp commands sends the
-- expected ByteString.
evalTestSeq :: (String, MockSmtp (), BS.ByteString) -> Expectation
evalTestSeq (_, a, expectBS) =
  runMockSmtp a `shouldBe` expectBS


dataSpec :: SpecWith ()
dataSpec =
  describe "data_" $
    it "should pass on its parameter unaltered" $ property $ \xs ->
      let bs = BSC.pack xs
          output = runMockSmtp $ data_ bs
          expected_output = "DATA" <> crlf <> bs
      in  expected_output == output

sendRawMailSpec :: SpecWith ()
sendRawMailSpec =
  describe "sendRawMail" $ do
    context "when passed content that does NOT end in crlf" $
      it "will add a CRLF (and the '.CRLF' terminator)" $ property $ \xs ->
          let bs = BSC.pack $ xs <> "|" -- our input doesn't end in crlf
              output = runMockSmtp $ 
                          sendRawMail "sender@s.com" ["recipient@r.com"] bs
              expected_output =
                    "MAIL FROM:<sender@s.com>\r\n"
                    <> "RCPT TO:<recipient@r.com>\r\n"
                    <> "DATA\r\n"
                    <> bs <> "\r\n.\r\n"
          in  expected_output == output
    context "when passed content that DOES end in crlf" $
      it "will add only the '.CRLF' terminator" $ property $ \xs ->
        let bs = BSC.pack $ xs <> crlf -- our input ends in crlf
            output = runMockSmtp $ 
                        sendRawMail "sender@s.com" ["recipient@r.com"] bs
            expected_output =
                    "MAIL FROM:<sender@s.com>\r\n"
                    <> "RCPT TO:<recipient@r.com>\r\n"
                    <> "DATA\r\n"
                    <> bs <> ".\r\n"
        in  expected_output == output

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "executing sequences of MonadSmtp commands" $
    describe "should send expected ByteStrings" $
      forM_ testSequences $ \s@(descrip, _, _) ->
          it descrip $ evalTestSeq s
  dataSpec
  sendRawMailSpec

