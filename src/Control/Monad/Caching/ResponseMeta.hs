{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Caching.ResponseMeta
-- Copyright   :  (C) 2015 Oleg Grenrus
-- License     :  BSD-3-Clause (see the file LICENSE)
--
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- 'Response' metadata, which we can serialised.
--
 ----------------------------------------------------------------------------
module Control.Monad.Caching.ResponseMeta (ResponseMeta(..), toResponseMeta, fromResponseMeta) where

import Control.Applicative
import Control.DeepSeq
import Data.Binary
import Data.ByteString as S
import Data.CaseInsensitive as CI
import Data.Data
import GHC.Generics
import Network.HTTP.Client.Internal
import Network.HTTP.Types
import Test.QuickCheck

-- | 'Response' meta data fields.
data ResponseMeta = ResponseMeta
    { rmStatus  :: !Status
    , rmVersion :: !HttpVersion
    , rmHeaders :: !ResponseHeaders
    }
    deriving (Eq, Ord, Show, Typeable, Generic)

instance NFData ResponseMeta where
    rnf (ResponseMeta (Status c m) (HttpVersion major minor) hdrs) =
        rnf c `seq`
        rnf m `seq`
        rnf major `seq`
        rnf minor `seq`
        rnf hdrs

toResponseMeta :: Response body -> ResponseMeta
toResponseMeta res = ResponseMeta
    { rmStatus = responseStatus res
    , rmVersion = responseVersion res
    , rmHeaders = responseHeaders res
    }

fromResponseMeta :: ResponseMeta
                 -> body       -- ^ Body
                 -> IO ()      -- ^ Close action
                 -> Response body
fromResponseMeta meta body close = Response
    { responseStatus     = rmStatus meta
    , responseVersion    = rmVersion meta
    , responseHeaders    = rmHeaders meta
    , responseBody       = body
    , responseCookieJar  = createCookieJar []
    , responseClose'     = ResponseClose close
    }

-- So we can more easily write Arbitrary instance
newtype SByteString = SByteString { getSByteString :: ByteString }
  deriving (Generic)

instance Arbitrary SByteString where
    arbitrary = SByteString . S.pack <$> arbitrary
    shrink    = Prelude.map (SByteString . S.pack) . shrink . (S.unpack . getSByteString)

instance Binary SByteString

type ResponseMetaTuple = ((Int, SByteString), (Int, Int), [(SByteString, SByteString)])

toTuple :: ResponseMeta -> ResponseMetaTuple
toTuple (ResponseMeta (Status code msg) (HttpVersion minor major) headers) =
    ((code, SByteString msg), (major, minor), Prelude.map f headers)
    where f (n, v) = (SByteString $ CI.original n, SByteString v)

fromTuple :: ResponseMetaTuple -> ResponseMeta
fromTuple ((code, msg), (major, minor), headers) =
    (ResponseMeta (Status code $ getSByteString msg) (HttpVersion minor major) (Prelude.map f headers))
    where f (n, v) = (CI.mk $ getSByteString n, getSByteString v)

instance Arbitrary ResponseMeta where
    arbitrary = fromTuple <$> arbitrary
    shrink    = Prelude.map fromTuple . shrink . toTuple

instance Binary ResponseMeta where
    put = put . toTuple
    get = fromTuple <$> get
