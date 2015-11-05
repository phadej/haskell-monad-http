{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.HTTP
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- 'MonadHttp' class with basic HTTP functionality.
----------------------------------------------------------------------------
module Control.Monad.Http (
    -- * Class
    MonadHttp(..),
    BodyReaderM,
    -- * Transformer
    HttpT(..),
    -- * Utilities
    httpLbs,
    brConsume,
    -- * Re-exports
    Request(..),
    Response(..),
    ) where

import Control.Monad.Trans.Http
import Control.Monad.Http.Class
import Network.HTTP.Client (Request(..), Response(..))
