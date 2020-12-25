{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Yesod.Katip
Description : Wrappers for adding automatic Katip integration to Yesod sites.
Copyright   : (c) Isaac van Bakel, 2020
License     : BSD3
Maintainer  : ivb@vanbakel.io
Stability   : experimental
Portability : POSIX

Katip's structured logging is useful, but adding logging after-the-fact to a
Yesod site which already uses the Yesod-provided logging invocations can be a
lot of work.

This module provides several convenience wrappers for converting existing Yesod
sites into Katip-using versions without needing to modify any handlers.
Instead, the wrapped versions will add in HTTP structures like requests, etc.
automatically, and logs sent to Yesod will be intercepted and also sent to
Katip along with any structure.

These wrappers are configurable - they can be made to redirect logs, duplicate
them (sending both to Katip and the Yesod logger), or even ignore them, as
necessary. See 'KatipConfig' for more detail.

If your site has a 'Yesod' instance, so will the wrapped version - so using it
is as simple as passing the wrapped version along to WAI, or whichever server
you use.

There's also support for using Katip's API for more direct control over your
Katip logs inside Yesod handlers. This is based in 'SiteKatip', which is a
ytl-style site class.
-}
module Yesod.Katip
  ( KatipSite (..)
  , KatipContextSite (..)

  , KatipConfig (..)
  , LoggingApproach (..)
  ) where

import Yesod.Katip.Class

import qualified Katip as K
import Network.Wai (Request)
import Yesod.Core
  ( RenderRoute (..)
  , waiRequest
  , Yesod (..)
  )
import Yesod.Core.Types
import Yesod.Site.Class
import Yesod.Site.Util
import Yesod.Trans.Class as ST
import Yesod.Trans.Class.Reader
import Yesod.Trans.TH

import Control.Monad (guard)
import Control.Monad.Logger as L
  ( Loc
  , LogSource
  , LogLevel (..)
  , LogStr
  , fromLogStr
  )
import Data.Bifunctor (second)
import Data.Default
import Data.Maybe (fromMaybe)

-- | Control how the Katip wrapper directs logs that come from Yesod.
--
-- Regardless of the choice of approach, logs will only be sent when
-- @shouldLogIO@ says they should.
data LoggingApproach
  = YesodOnly
  -- ^ Send these logs only to the Yesod logger configured by the site's Yesod
  -- instance already. This is provided only for debugging convenience - it
  -- doesn't make sense to use it in production.
  | KatipOnly
  -- ^ Send these logs only to the Katip scribes, ignoring the Yesod
  -- logger.
  | Both
  -- ^ Send logs to both the Katip scribes and the Yesod logger. If Katip is
  -- configured to log structure as well, this structure *won't* be sent to the
  -- Yesod logger. This is the default.

-- | Configuration for how 'KatipSite' and 'KatipContextSite' turn Yesod logs
-- into Katip ones
data KatipConfig
  = KatipConfig
      { loggingApproach :: LoggingApproach
      -- ^ How logs should be sent between the Yesod logger and your Katip scribes.
      -- See 'LoggingApproach' for details.
      , levelToSeverity :: LogLevel -> K.Severity
      -- ^ How a Yesod level should be translated into a Katip severity.
      , sourceToNamespace :: LogSource -> K.Namespace
      -- ^ How a Yesod log source should modify the Katip namespace. By default,
      -- it is appended on.
      }

instance Default KatipConfig where
  def = KatipConfig
          { loggingApproach = Both 
          , levelToSeverity = defaultLevelToSeverity
          , sourceToNamespace = K.Namespace . pure
          }

defaultLevelToSeverity :: LogLevel -> K.Severity
defaultLevelToSeverity LevelDebug = K.DebugS
defaultLevelToSeverity LevelInfo = K.InfoS
defaultLevelToSeverity LevelWarn = K.WarningS
defaultLevelToSeverity LevelError = K.ErrorS
defaultLevelToSeverity (LevelOther other) = fromMaybe K.ErrorS $ K.textToSeverity other

-------------------
--- CONVERSIONS ---
-------------------

-- Bridge between Yesod-style logging and Katip-style

katipLog :: KatipConfig -> K.LogEnv -> Loc -> LogSource -> LogLevel -> L.LogStr -> IO ()
katipLog KatipConfig{..} logEnv loc source level str = do
  K.runKatipT logEnv do
    K.logItem () (sourceToNamespace source) (Just loc) (levelToSeverity level) (K.logStr $ L.fromLogStr str)

katipLogWithContexts
  :: KatipConfig -> K.LogEnv -> K.LogContexts -> K.Namespace
  -> Loc -> LogSource -> LogLevel -> L.LogStr -> IO ()
katipLogWithContexts KatipConfig{..} logEnv logCtxts namespace loc source level str = do
  K.runKatipContextT logEnv logCtxts namespace do
    K.logItem logCtxts (namespace <> sourceToNamespace source) (Just loc)
      (levelToSeverity level) (K.logStr $ L.fromLogStr str)

---------------
--- LOGGING ---
---------------

-- A Katip wrapper for logging to Katip from Yesod

-- | A wrapper for adding Katip functionality to a site.
--
-- This is the most basic wrapper. It will allow you to redirect logs from
-- Yesod to Katip, as configured.  It will not include HTTP structures in the
-- output - for that, look at 'KatipContextSite' instead.
newtype KatipSite site
  = KatipSite
      { unKatipSite :: ReaderSite (KatipConfig, K.LogEnv) site
      }

instance SiteTrans KatipSite where
  lift = withSiteT unKatipSite . lift

  mapSiteT runner = withSiteT unKatipSite . mapSiteT runner . withSiteT KatipSite

instance (RenderRoute site, Eq (Route site)) => RenderRoute (KatipSite site) where
  newtype Route (KatipSite site) = KRoute (Route (ReaderSite (KatipConfig, K.LogEnv) site))
  renderRoute (KRoute route) = renderRoute route

instance SiteKatip (KatipSite site) where
  getLogEnv = withSiteT unKatipSite $ snd <$> ask

  localLogEnv f = withSiteT unKatipSite . (local $ second f) . withSiteT KatipSite

deriving instance Eq (Route site) => Eq (Route (KatipSite site))

defaultYesodInstanceExcept [| unReaderSite . unKatipSite |] [d|
    instance (SiteCompatible site (KatipSite site), Yesod site, Eq (Route site)) => Yesod (KatipSite site) where
      messageLoggerSource (KatipSite (ReaderSite (config, env) site)) logger loc source level str = do
        shouldLog <- shouldLogIO site source level

        let KatipConfig { loggingApproach } = config
            logYesod = messageLoggerSource site logger loc source level str
            logKatip = do
              guard shouldLog
              katipLog config env loc source level str

        case loggingApproach of
          KatipOnly ->
            logKatip

          YesodOnly ->
            logYesod

          Both -> do
            logKatip
            logYesod
  |]

----------------------------
--- LOGGING WITH CONTEXT ---
----------------------------

-- The same thing again, only this one logs the context

-- | A wrapper for adding Katip functionality to a site.
--
-- This is the more featureful wrapper. It can redirect logs, just like
-- 'KatipSite', but will also augment them with useful HTTP structure from
-- Yesod.
data KatipContextSite site
  = KatipContextSite
      { unKatipContextSite :: ReaderSite (KatipConfig, K.LogEnv, K.LogContexts, K.Namespace) site
      }

instance SiteTrans KatipContextSite where
  lift = withSiteT unKatipContextSite . lift

  mapSiteT runner = withSiteT unKatipContextSite . mapSiteT runner . withSiteT KatipContextSite

instance SiteKatip (KatipContextSite site) where
  getLogEnv = withSiteT unKatipContextSite $ do
    (_, env, _, _) <- ask
    pure env

  localLogEnv f = withSiteT unKatipContextSite . local (\(a, env, c, d) -> (a, f env, c, d)) . withSiteT KatipContextSite

instance SiteKatipContext (KatipContextSite site) where
  getKatipContext = withSiteT unKatipContextSite $ do
    (_, _, ctxt, _) <- ask
    pure ctxt

  localKatipContext f = withSiteT unKatipContextSite . local (\(a, b, ctxt, d) -> (a, b, f ctxt, d)) . withSiteT KatipContextSite

  getKatipNamespace = withSiteT unKatipContextSite $ do
    (_, _, _, ns) <- ask
    pure ns
  localKatipNamespace f = withSiteT unKatipContextSite . local (\(a, b, c, ns) -> (a, b, c, f ns)) . withSiteT KatipContextSite

instance (RenderRoute site, Eq (Route site)) => RenderRoute (KatipContextSite site) where
  newtype Route (KatipContextSite site) = KCRoute (Route (ReaderSite (KatipConfig, K.LogEnv, K.LogContexts, K.Namespace) site))
  renderRoute (KCRoute route) = renderRoute route

deriving instance Eq (Route site) => Eq (Route (KatipContextSite site))

defaultYesodInstanceExcept [| unReaderSite . unKatipContextSite |] [d|
    instance (K.LogItem Request, SiteCompatible site (KatipContextSite site), Yesod site, Eq (Route site))
      => Yesod (KatipContextSite site) where
      messageLoggerSource (KatipContextSite (ReaderSite (config, env, context, namespace) site)) logger loc source level str = do
        shouldLog <- shouldLogIO site source level

        let KatipConfig { loggingApproach } = config
            logYesod = messageLoggerSource site logger loc source level str
            logKatip = do
              guard shouldLog
              katipLogWithContexts config env context namespace loc source level str

        case loggingApproach of
          KatipOnly ->
            logKatip

          YesodOnly ->
            logYesod

          Both -> do
            logKatip
            logYesod

      yesodMiddleware argM = do
        req <- waiRequest
        K.katipAddContext req $ mapSiteT yesodMiddleware argM
  |]

