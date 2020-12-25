{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Yesod.Katip.Class
Description : Class and instances for Katip logging in Yesod handlers
Copyright   : (c) Isaac van Bakel, 2020
License     : BSD3
Maintainer  : ivb@vanbakel.io
Stability   : experimental
Portability : POSIX

-}

module Yesod.Katip.Class
  ( SiteKatip (..)
  , SiteKatipContext (..)
  ) where

import qualified Katip as K
import Yesod.Site.Class
import Yesod.Trans.Class

-- | A class for sites which provide a 'Katip'-equivalent API
class SiteKatip site where
  getLogEnv :: (MonadSite m) => m site K.LogEnv
  localLogEnv :: (MonadSite m) => (K.LogEnv -> K.LogEnv) -> m site a -> m site a

-- | A class for sites which provide a 'KatipContext'-equivalent API
class SiteKatip site => SiteKatipContext site where
  getKatipContext :: (MonadSite m) => m site K.LogContexts
  localKatipContext :: (MonadSite m) => (K.LogContexts -> K.LogContexts) -> m site a -> m site a

  getKatipNamespace :: (MonadSite m) => m site K.Namespace
  localKatipNamespace :: (MonadSite m) => (K.Namespace -> K.Namespace) -> m site a -> m site a

instance {-# OVERLAPPABLE #-}
  (SiteTrans t, SiteKatip site) => SiteKatip (t site) where
  getLogEnv = lift getLogEnv
  localLogEnv = mapSiteT . localLogEnv

instance {-# OVERLAPPABLE #-}
  (SiteTrans t, SiteKatipContext site) => SiteKatipContext (t site) where
  getKatipContext = lift getKatipContext
  localKatipContext = mapSiteT . localKatipContext
  
  getKatipNamespace = lift getKatipNamespace
  localKatipNamespace = mapSiteT . localKatipNamespace

instance (MonadSite m, SiteKatip site) => K.Katip (m site) where
  getLogEnv = getLogEnv
  localLogEnv = localLogEnv

instance (MonadSite m, SiteKatipContext site) => K.KatipContext (m site) where
  getKatipContext = getKatipContext
  localKatipContext = localKatipContext

  getKatipNamespace = getKatipNamespace
  localKatipNamespace = localKatipNamespace
