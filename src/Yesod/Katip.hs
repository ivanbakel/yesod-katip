{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Yesod.Katip
  ( KatipSite (..)
  , KatipContextSite (..)
  ) where

import Data.Has
import Katip as K
import Yesod.Core
import Yesod.Core.Types

import Control.Monad.Logger as L (Loc, LogSource, LogLevel, LogStr, fromLogStr)
import Control.Monad.Reader
import Data.Maybe (fromMaybe)

newtype KatipSite site = KatipSite { unKatipSite :: site }
newtype KatipContextSite site = KatipContextSite { unKatipContextSite :: site }

wrapK :: HandlerFor site a -> HandlerFor (KatipSite site) a
wrapK (HandlerFor innerHandler) = HandlerFor \(HandlerData{..}) ->
  innerHandler $ HandlerData
    { handlerEnv = wrapKRHE handlerEnv
    , ..
    }
  where
    wrapKRHE :: RunHandlerEnv (KatipSite site) (KatipSite site) -> RunHandlerEnv site site
    wrapKRHE RunHandlerEnv{..}
      = RunHandlerEnv
          { rheRender = rheRender . KRoute 
          , rheRoute = unKRoute <$> rheRoute
          , rheRouteToMaster = unKRoute . rheRouteToMaster . KRoute
          , rheSite = unKatipSite rheSite
          , rheChild = unKatipSite rheSite
          , ..
          }

katipLog :: LogEnv -> Loc -> LogSource -> LogLevel -> L.LogStr -> IO ()
katipLog logEnv loc source level str = do
  runKatipT logEnv do
    logItem () (sourceToNamespace source) (Just loc) (levelToSeverity level) (K.logStr $ L.fromLogStr str)

katipLogWithContexts :: LogEnv -> LogContexts -> Namespace -> Loc -> LogSource -> LogLevel -> L.LogStr -> IO ()
katipLogWithContexts logEnv logCtxts namespace loc source level str = do
  runKatipContextT logEnv logCtxts namespace do
    logItem logCtxts namespace (Just loc) (levelToSeverity level) (K.logStr $ L.fromLogStr str)

levelToSeverity :: LogLevel -> Severity
levelToSeverity LevelDebug = DebugS
levelToSeverity LevelInfo = InfoS
levelToSeverity LevelWarn = WarningS
levelToSeverity LevelError = ErrorS
levelToSeverity (LevelOther other) = fromMaybe ErrorS $ textToSeverity other

sourceToNamespace :: LogSource -> Namespace
sourceToNamespace source = Namespace [source]

instance (Has a site) => (Has a (KatipSite site)) where
  getter  = getter . unKatipSite
  modifier mod = KatipSite . modifier mod . unKatipSite
  hasLens f = (KatipSite <$>) . hasLens f . unKatipSite

instance (RenderRoute site, Eq (Route site)) => RenderRoute (KatipSite site) where
  data Route (KatipSite site) = KRoute { unKRoute :: Route site }
  renderRoute (KRoute route) = renderRoute @site route

deriving instance Eq (Route site) => Eq (Route (KatipSite site))

instance (Yesod site, Eq (Route site), Has LogEnv site) => Yesod (KatipSite site) where
  approot
    = case approot @site of
        ApprootRelative -> ApprootRelative
        ApprootStatic root -> ApprootStatic root
        ApprootMaster f -> ApprootMaster (f . unKatipSite)
        ApprootRequest f -> ApprootRequest (f . unKatipSite)
        -- Because new cases may be added without a major bump, we need a
        -- fallback case for version compatibility
        _ -> guessApproot

  errorHandler err = do
    wrapK (errorHandler err)

  urlParamRenderOverride (KatipSite site) (KRoute route) query
    = urlParamRenderOverride site route query

  isAuthorized (KRoute route) = wrapK . isAuthorized route 

  isWriteRequest = wrapK . isWriteRequest . unKRoute

  authRoute (KatipSite site)
    = KRoute <$> authRoute site

  cleanPath = cleanPath . unKatipSite

  joinPath = joinPath . unKatipSite

  maximumContentLength (KatipSite site) mRoute
    = maximumContentLength site (unKRoute <$> mRoute)

  maximumContentLengthIO (KatipSite site) mRoute
    = maximumContentLengthIO site (unKRoute <$> mRoute)

  makeLogger = makeLogger . unKatipSite

  messageLoggerSource (KatipSite site) logger loc source level str = do
    shouldLog <- shouldLogIO site source level

    if shouldLog
      then katipLog (getter site) loc source level str
      else pure ()

    messageLoggerSource site logger loc source level str

  jsAttributes = jsAttributes . unKatipSite

  jsAttributesHandler = wrapK jsAttributesHandler

  makeSessionBackend = makeSessionBackend . unKatipSite

  fileUpload = fileUpload . unKatipSite

  shouldLogIO = shouldLogIO . unKatipSite

instance (RenderRoute site, Eq (Route site)) => RenderRoute (KatipContextSite site) where
  data Route (KatipContextSite site) = KCRoute (Route site)
  renderRoute (KCRoute route) = renderRoute @site route

deriving instance Eq (Route site) => Eq (Route (KatipContextSite site))

instance (Yesod site, Eq (Route site), Has LogEnv site, Has LogContexts site, Has Namespace site) => Yesod (KatipContextSite site) where
  messageLoggerSource (KatipContextSite site) logger loc source level str = do
    shouldLog <- shouldLogIO site source level

    if shouldLog
      then katipLogWithContexts (getter site) (getter site) (getter site) loc source level str
      else pure ()

    messageLoggerSource site logger loc source level str
