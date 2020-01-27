
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{- |

Provides 'MockSmtp', a 'mock' instance of 'MonadSmtp', which simply
records (via use of a 'Writer') any bytes sent through it.
(Making it technically a \"spy", I think --
see <https://martinfowler.com/articles/mocksArentStubs.html>.)

Plus a transformer version, 'MockSmtpT'.

Sample use:

>>> runMockSmtp (noop >> quit)
"NOOP\r\nQUIT\r\n"

-}

module Network.Mail.Assumpta.Mock
  where

import Control.Monad.Writer
import Data.Functor.Identity (Identity)
import Data.ByteString ( ByteString )

import Network.Mail.Assumpta.MonadSmtp

-- | Concrete transformer for mock 'MonadSmtp'
-- monads. 
newtype MockSmtpT m a = MockSmtpT {
    runMockSmtpT :: WriterT ByteString m a
  } 
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix
            , MonadWriter ByteString)

-- | Lift into 'MockSmtpT'.
liftMockSmtpT :: Monad m => m a -> MockSmtpT m a
liftMockSmtpT = MockSmtpT . lift

instance MonadTrans MockSmtpT where
  lift = liftMockSmtpT

-- | 'MockSmtp' specialized to 'Identity' 
type MockSmtp = MockSmtpT Identity

-- | In this mock monad, 'send' writes to the underlying 'Writer';
-- 'expectCode' and 'tlsUpgrade' are no-ops; and 'getReply'
-- returns an empty list. (In breach of the req. that a
-- reply always contains at least one line.)
instance Monad m => MonadSmtp (MockSmtpT m) where
  send        = MockSmtpT . tell
  getReply    = return []
  expectCode  = const $ pure ()
  tlsUpgrade  = pure ()

-- | Run an 'MonadSmtp' computation using a mock,
-- and return the 'ByteString' content written.
runMockSmtp :: MockSmtp a -> ByteString
runMockSmtp a = execWriter $ runMockSmtpT a

