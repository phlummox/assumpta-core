
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 801
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
#endif

{-# OPTIONS_HADDOCK show-extensions #-}

{- |

This module provides a template for creating implementations
of 'MonadSmtp' over some abstract connection type.

Operations on a connection can throw IO- and network-based errors,
and we don't handle those in any particular way
ourselves. However, we throw 'SmtpError's if server responses
are unparseable or unexpected.

-}

module Network.Mail.Assumpta.Trans.Smtp
  (
  -- * Abstract connections
    module Conn

  -- * SMTP operations
  --
  -- A monad transformer, 'SmtpT', which provides the ability to
  -- send SMTP commands and parse replies, plus operations
  -- on the transformer.
  , SmtpT(..)
  , liftSmtpT
  , mapSmtpT
  , MonadSmtp
  -- ** run SmptT actions
  , runSmtpEither 
  , runSmtp
  , withSmtpConnection
  -- * Utility functions
  , rethrow
  )
  where

import Control.Monad.Catch (bracket, MonadMask)
import Control.Monad.Except
import Control.Monad.Reader

import Network.Mail.Assumpta.Connection     as Conn
import Network.Mail.Assumpta.Types
import Network.Mail.Assumpta.MonadSmtp      as MonadSmtp
import Network.Mail.Assumpta.ParseResponse  as P (getReply)

{-# ANN module ("HLint: ignore Eta reduce" :: String) #-}

-- | Monad transformer that adds the ability to send SMTP
-- commands and receive server replies over some abstract
-- communications channel, \'conn'.
newtype SmtpT conn m a = SmtpT {
    unSmtpT :: ReaderT conn (ExceptT SmtpError m) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix
           , MonadError SmtpError, MonadReader conn
           )

-- | An instance of 'MonadSmtp' communicating over
-- some 'Connection' type, @conn@.
instance (Connection conn, cstr ~ Cstrt conn, Monad m, cstr (SmtpT conn m)) 
  => MonadSmtp.MonadSmtp (SmtpT conn m)
  where
    send bs     = ask >>= (`Conn.send` bs)
    getReply    = asks recv >>= P.getReply
    tlsUpgrade  = ask >>= upgrade

-- | 'lift', specialised to the 'SmtpT' transformer.
liftSmtpT :: Monad m => m a -> SmtpT conn m a
liftSmtpT = SmtpT . lift . lift

instance MonadTrans (SmtpT conn) where
  lift = liftSmtpT

-- | convert an ExceptT into a MonadError
rethrow :: MonadError e m => ExceptT e m b -> m b
rethrow =  (>>= either throwError return) . runExceptT


-- | Lifted 'mapExceptT'.
mapSmtpT ::
  (m1 (Either SmtpError a1) -> m2 (Either SmtpError a2))
  -> SmtpT conn m1 a1 -> SmtpT conn m2 a2
mapSmtpT f (SmtpT x) = SmtpT (mapBoth f x)
  where
    mapBoth = mapReaderT . mapExceptT

-- | @runSmtpEither c a@
--
-- Run an 'SmtpT' computation @a@ using some connection @c@,
-- and return the result as an 'Either'.
runSmtpEither :: conn -> SmtpT conn m a -> m (Either SmtpError a)
runSmtpEither c =  runExceptT . flip runReaderT c . unSmtpT

-- | @runSmtp c a@
--
-- 'runSmtpEither' generalized to 'MonadError', so
-- a caller can 
-- use 'Maybe' or or 'MonadError' instances as they choose.
runSmtp :: MonadError SmtpError m => conn -> SmtpT conn m b -> m b
runSmtp c  = 
  rethrow . flip runReaderT c . unSmtpT

-- | 'withConnection', specialized to only run
-- 'MonadSmtp.MonadSmtp' actions.
withSmtpConnection
  :: (Cstrt c m, MonadMask m, Connection c, MonadSmtp.MonadSmtp m) =>
     Params c -> (c -> m b) -> m b
withSmtpConnection = withConnection

-- | @withSmtpRunner params r a@
--
-- A variant on 'withSmtpConnection',
-- where the caller supplies a \'runner' @r@, which can
-- run 'MonadSmtp.MonadSmtp' actions @a@ and return a result
-- in the current monad.
withSmtpRunner ::
  (MonadMask m, Cstrt conn m, Connection conn) =>
  Params conn -> (conn -> (forall n . MonadSmtp.MonadSmtp n => n b) -> m b) -> (forall n . MonadSmtp.MonadSmtp n => n b) -> m b
withSmtpRunner params f a  =
    bracket acquire release (`f` a)
  where
    acquire   = open params 
    release   = close

