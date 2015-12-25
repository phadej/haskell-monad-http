{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
module Control.Monad.Trans.Http (
    HttpT(..),
    evalHttpT,
    mapHttpT,
    liftHttpT,
    ) where

import Prelude        ()
import Prelude.Compat

import qualified Network.HTTP.Client     as H
import qualified Network.HTTP.Client.TLS as H

import Control.Monad.Cont.Class   (MonadCont (..))
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.Reader.Class (MonadReader (..))
import Control.Monad.RWS.Class    (MonadRWS)
import Control.Monad.State.Class  (MonadState (..))
import Control.Monad.Trans.Class  (MonadTrans (..))
import Control.Monad.Writer.Class (MonadWriter (..))

#if MIN_VERSION_mtl(2,2,0)
import Control.Monad.Except (MonadError (..))
#else
import Control.Monad.Error (MonadError (..))
#endif

import Control.Monad.Catch  (MonadCatch (..), MonadMask (..), MonadThrow (..))
import Control.Monad.Logger (MonadLogger (..), MonadLoggerIO (..))

import Control.Monad.Random.Class (MonadRandom (..), MonadSplit (..))

import Control.Monad.CryptoRandom (MonadCRandom (..), MonadCRandomR (..))

-- | Http monad transformer, essentially 'ReaderT' 'H.Manager'.
newtype HttpT m a = HttpT { runHttpT :: H.Manager -> m a }

-- | Lower 'HttpT' with default 'H.Manager' created with 'H.tlsManagerSettings'.
evalHttpT :: MonadIO m => HttpT m a -> m a
evalHttpT m = liftIO (H.newManager H.tlsManagerSettings) >>= runHttpT m

instance Functor m => Functor (HttpT m) where
    fmap f = mapHttpT (fmap f)

instance Applicative m => Applicative (HttpT m) where
    pure    = liftHttpT . pure
    f <*> v = HttpT $ \r -> runHttpT f r <*> runHttpT v r

instance Monad m => Monad (HttpT m) where
    return = liftHttpT . return
    m >>= k  = HttpT $ \r -> do
        a <- runHttpT m r
        runHttpT (k a) r

instance MonadIO m => MonadIO (HttpT m) where
    liftIO = liftHttpT . liftIO

instance MonadThrow m => MonadThrow (HttpT m) where
    throwM = liftHttpT . throwM

instance MonadCatch m => MonadCatch (HttpT m) where
    catch m c = HttpT $ \r -> runHttpT m r `catch` \e -> runHttpT (c e) r

instance MonadMask m => MonadMask (HttpT m) where
    mask a = HttpT $ \r -> mask $ \u -> runHttpT (a $ mapHttpT u) r
    uninterruptibleMask a =
        HttpT $ \r -> uninterruptibleMask $ \u -> runHttpT (a $ mapHttpT u) r

instance MonadLogger m => MonadLogger (HttpT m) where
    monadLoggerLog a b c d = liftHttpT $ monadLoggerLog a b c d

instance MonadLoggerIO m => MonadLoggerIO (HttpT m) where
    askLoggerIO = liftHttpT askLoggerIO

instance MonadTrans HttpT where
    lift = liftHttpT

instance MonadReader r m => MonadReader r (HttpT m) where
  ask = lift ask
  local = mapHttpT . local

instance MonadState s m => MonadState s (HttpT m) where
    get = lift get
    put = lift . put

instance MonadCont m => MonadCont (HttpT m) where
    callCC f = HttpT $ \i -> callCC $ \c -> runHttpT (f (HttpT . const . c)) i

instance MonadError e m => MonadError e (HttpT m) where
    throwError = lift . throwError
    catchError r h =
        HttpT $ \i -> runHttpT r i `catchError` \e -> runHttpT (h e) i

instance MonadWriter w m => MonadWriter w (HttpT m) where
    tell   = lift . tell
    listen = mapHttpT listen
    pass   = mapHttpT pass

instance MonadRWS r w s m => MonadRWS r w s (HttpT m)

instance MonadRandom m => MonadRandom (HttpT m) where
    getRandom = lift getRandom
    getRandoms = lift getRandoms
    getRandomR = lift . getRandomR
    getRandomRs = lift . getRandomRs

instance MonadSplit g m => MonadSplit g (HttpT m) where
    getSplit = lift getSplit

instance MonadCRandom e m => MonadCRandom e (HttpT m) where
    getCRandom = lift getCRandom
    getBytes = lift . getBytes
    getBytesWithEntropy = \i -> lift . getBytesWithEntropy i
    doReseed = lift . doReseed

instance MonadCRandomR e m => MonadCRandomR e (HttpT m) where
    getCRandomR = lift . getCRandomR

mapHttpT :: (m a -> m b) -> HttpT m a -> HttpT m b
mapHttpT f m = HttpT $ f . runHttpT m

liftHttpT :: m a -> HttpT m a
liftHttpT = HttpT . const
