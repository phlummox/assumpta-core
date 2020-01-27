
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Instances for mtl classes which require UndecidableInstances.

-}

module Network.Mail.Assumpta.Instances
  where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer

import Network.Mail.Assumpta.Trans.Smtp

instance MonadWriter w m => MonadWriter w (SmtpT conn m) where
    writer = lift . writer
    tell   = lift . tell
    listen (SmtpT m) = SmtpT $ listen m
    pass (SmtpT m) = SmtpT $ pass m

instance (MonadReader r m) => MonadReader r (SmtpT conn m) where
    ask = liftSmtpT ask
    local = mapSmtpT . local

instance MonadState s m => MonadState s (SmtpT conn m) where
    get = lift get
    put = lift . put
    state = lift . state

-- TODO: MonadError.
--
-- instance MonadError e m => MonadError e (SmtpT conn m) where
--   throwError = lift . throwError
-- 
--   catchError :: SmtpT conn m a -> (e -> SmtpT conn m a) -> SmtpT conn m a
--   catchError = ...

-- see eg WriterT:
-- -- | Lift a @catchE@ operation to the new monad.
-- liftCatch :: Catch e m (a,w) -> Catch e (WriterT w m) a
-- liftCatch catchE m h =
--     WriterT $ runWriterT m `catchE` \ e -> runWriterT (h e)


