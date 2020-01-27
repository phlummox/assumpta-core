
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}

{-# OPTIONS_HADDOCK show-extensions #-}

{- |

Class using type families to represent abstract connection types
that (in addition to the usual network operations)
can be upgraded from insecure to secure.

Any instance must specify:

*   @'Params'@, the type of parameters that can be used to
    open a connection. (This might be a just a tuple containing
    just hostname and port, say, or might be some more complicated
    structure.)

*   @'Cstrt'@, the constraints the methods require. For instance,
    a 'Connection' using real network operations would likely
    have 'MonadIO' as a constraint.

*   Implementations for 'open', 'close', 'send', 'recv' and 'upgrade'. 
    These are typically very thin wrappers around the operations
    provided by a network library.
-}

module Network.Mail.Assumpta.Connection
  where

import Control.Monad.Catch
import Data.ByteString ( ByteString )
import Data.Constraint

-- | Class for abstract connections over some
-- transport channel @c@.
class Connection c where
  -- | Constraints an implementation must satisfy.
  -- (e.g., 'MonadIO' for network-based connections.)
  type Cstrt c :: (* -> *) -> Constraint
  -- | Parameters used to create a connection --
  -- e.g. hostname, port, TLS settings, etc.
  type Params c :: *

  open    :: (Cstrt c) m => Params c -> m c         -- ^ open a connection
  close   :: (Cstrt c) m => c -> m ()               -- ^ close a connection
  send    :: (Cstrt c) m => c -> ByteString -> m () -- ^ send a bytestring
  recv    :: (Cstrt c) m => c -> m ByteString       -- ^ receive a bytestring
  upgrade :: (Cstrt c) m => c -> m ()               -- ^ upgrade security


-- | @withConnection p a@
--
-- 'bracket' for 'Connection's.
-- Given some parameters @p@ for opening a connection: 
-- open a connection, run some action @a@ with it, then
-- close.
withConnection ::
  (MonadMask m, Cstrt c m, Connection c) =>
  Params c -> (c -> m b) -> m b
withConnection params =
    bracket acquire release
  where
    acquire   = open params
    release   = close

