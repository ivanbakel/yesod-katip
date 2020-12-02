{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
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
  ( KatipSite
  , wrapK
  , insideK

  , KatipContextSite
  , wrapKC
  , insideKC

  , LoggingApproach (..)
  ) where

import Data.Has
import Katip as K
import Yesod.Core
import Yesod.Core.Types

import Control.Monad.Logger as L (Loc, LogSource, LogLevel, LogStr, fromLogStr)
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Maybe (fromMaybe)

import Yesod.Katip.Internal.Singletons

-------------------
--- CONVERSIONS ---
-------------------

-- Bridge between Yesod-style logging and Katip-style

katipLog :: LogEnv -> Loc -> LogSource -> LogLevel -> L.LogStr -> IO ()
katipLog logEnv loc source level str = do
  runKatipT logEnv do
    logItem () (Namespace []) (Just loc) (levelToSeverity level) (K.logStr $ L.fromLogStr str)

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

---------------
--- LOGGING ---
---------------

-- A Katip wrapper for logging to Katip from Yesod

data KatipSite (loggingApproach :: LoggingApproach) site
  = KatipSite
      { unKatipSite :: site
      }
  deriving Functor

-- Here is where the singletons become necessary - in order to run 'handler'
-- correctly, we actually need to *wrap* in a 'KatipSite' at this point -
-- but if the logging approach wasn't in the type, we would need to pick one
-- with no information to produce the wrapping.
wrapKRHE :: RunHandlerEnv site site
         -> RunHandlerEnv (KatipSite loggingApproach site) (KatipSite loggingApproach site)
wrapKRHE RunHandlerEnv{..}
  = RunHandlerEnv
      { rheRender = rheRender . unKRoute
      , rheRoute = KRoute <$> rheRoute
      , rheRouteToMaster = KRoute . rheRouteToMaster . unKRoute
      , rheSite = KatipSite rheSite
      , rheChild = KatipSite rheSite
      , ..
      }

unwrapKRHE :: RunHandlerEnv (KatipSite loggingApproach site) (KatipSite loggingApproach site)
           -> RunHandlerEnv site site
unwrapKRHE RunHandlerEnv{..}
  = RunHandlerEnv
      { rheRender = rheRender . KRoute
      , rheRoute = unKRoute <$> rheRoute
      , rheRouteToMaster = unKRoute . rheRouteToMaster . KRoute
      , rheSite = unKatipSite rheSite
      , rheChild = unKatipSite rheSite
      , ..
      }

wrapK :: HandlerFor site a -> HandlerFor (KatipSite loggingApproach site) a
wrapK (HandlerFor innerHandler) = HandlerFor \(HandlerData{..}) ->
  innerHandler $ HandlerData
    { handlerEnv = unwrapKRHE handlerEnv
    , ..
    }

unwrapK :: HandlerFor (KatipSite loggingApproach site) a
        -> HandlerFor site a
unwrapK (HandlerFor outerHandler) = HandlerFor \(HandlerData{..}) ->
  outerHandler $ HandlerData
    { handlerEnv = wrapKRHE handlerEnv
    , ..
    }

insideK :: (HandlerFor site a -> HandlerFor site b)
        -> HandlerFor (KatipSite loggingApproach site) a
        -> HandlerFor (KatipSite loggingApproach site) b
insideK handler
  = wrapK . handler . unwrapK

wrapKW :: WidgetFor site a -> WidgetFor (KatipSite loggingApproach site) a
wrapKW (WidgetFor innerWidget)
  -- TODO: figure out how best to do this. 'tellWidget' looks like the
  -- right approach, but its semantics need checking
  = error "This has not yet been implemented!"

unwrapKW :: WidgetFor (KatipSite loggingApproach site) a -> WidgetFor site a
unwrapKW (WidgetFor outerWidget)
  -- TODO: ditto
  = error "This has not yet been implemented!"

instance (Has a site) => (Has a (KatipSite loggingApproach site)) where
  getter  = getter . unKatipSite
  modifier mod = (modifier mod <$>)
  hasLens f = (KatipSite <$>) . hasLens f . unKatipSite

instance (RenderRoute site, Eq (Route site)) => RenderRoute (KatipSite loggingApproach site) where
  data Route (KatipSite loggingApproach site) = KRoute { unKRoute :: Route site }
  renderRoute (KRoute route) = renderRoute @site route

deriving instance Eq (Route site) => Eq (Route (KatipSite loggingApproach site))

instance (Yesod site, Eq (Route site), Has LogEnv site, SingLoggingApproach loggingApproach)
          => Yesod (KatipSite loggingApproach site) where
  approot
    = case approot @site of
        ApprootRelative -> ApprootRelative
        ApprootStatic root -> ApprootStatic root
        ApprootMaster f -> ApprootMaster (f . unKatipSite)
        ApprootRequest f -> ApprootRequest (f . unKatipSite)
        -- Because new cases may be added without a major bump, we need a
        -- fallback case for version compatibility
        _ -> guessApproot

  errorHandler err = wrapK (errorHandler err)

  defaultLayout = wrapK . defaultLayout . unwrapKW

  urlParamRenderOverride (KatipSite site) (KRoute route) query
    = urlParamRenderOverride site route query

  isAuthorized (KRoute route) = wrapK . isAuthorized route

  isWriteRequest = wrapK . isWriteRequest . unKRoute

  authRoute (KatipSite site)
    = KRoute <$> authRoute site

  cleanPath = cleanPath . unKatipSite

  joinPath = joinPath . unKatipSite

  addStaticContent ext mime content
    = ((first KRoute <$>) <$>) <$> wrapK (addStaticContent ext mime content)

  maximumContentLength (KatipSite site) mRoute
    = maximumContentLength site (unKRoute <$> mRoute)

  maximumContentLengthIO (KatipSite site) mRoute
    = maximumContentLengthIO site (unKRoute <$> mRoute)

  makeLogger = makeLogger . unKatipSite

  messageLoggerSource (KatipSite site) logger loc source level str = do
    shouldLog <- shouldLogIO site source level

    let logYesod = messageLoggerSource site logger loc source level str
        logKatip = do
          guard shouldLog
          katipLog (getter site) loc source level str
        loggingApproach = fromSLoggingApproach (singLoggingApproach @loggingApproach)

    case loggingApproach of
      KatipOnly ->
        logKatip

      YesodOnly ->
        logYesod

      Both -> do
        logKatip
        logYesod

  jsLoader (KatipSite site)
    = case jsLoader site of
        BottomOfBody -> BottomOfBody
        BottomOfHeadBlocking -> BottomOfHeadBlocking
        BottomOfHeadAsync asyncLoader ->
          BottomOfHeadAsync \urls mAsyncWidget widgetToMake ->
            let mInnerWidget = (\generator renderer -> generator (renderer . unKRoute)) <$> mAsyncWidget
            in
              asyncLoader urls mInnerWidget (widgetToMake . KRoute)

  jsAttributes = jsAttributes . unKatipSite

  jsAttributesHandler = wrapK jsAttributesHandler

  makeSessionBackend = makeSessionBackend . unKatipSite

  fileUpload = fileUpload . unKatipSite

  shouldLogIO = shouldLogIO . unKatipSite

  yesodMiddleware = insideK yesodMiddleware

  yesodWithInternalState (KatipSite site) mRoute = yesodWithInternalState site (unKRoute <$> mRoute)

  defaultMessageWidget snippet widget
    = wrapKW (defaultMessageWidget snippet (\renderer -> widget (renderer . unKRoute)))

----------------------------
--- LOGGING WITH CONTEXT ---
----------------------------

-- The same thing again, only this one logs the context

data KatipContextSite (loggingApproach :: LoggingApproach) site
  = KatipContextSite
      { unKatipContextSite :: site
      }
  deriving Functor

wrapKCRHE :: RunHandlerEnv site site
         -> RunHandlerEnv (KatipContextSite loggingApproach site) (KatipContextSite loggingApproach site)
wrapKCRHE RunHandlerEnv{..}
  = RunHandlerEnv
      { rheRender = rheRender . unKCRoute
      , rheRoute = KCRoute <$> rheRoute
      , rheRouteToMaster = KCRoute . rheRouteToMaster . unKCRoute
      , rheSite = KatipContextSite rheSite
      , rheChild = KatipContextSite rheSite
      , ..
      }

unwrapKCRHE :: RunHandlerEnv (KatipContextSite loggingApproach site) (KatipContextSite loggingApproach site)
           -> RunHandlerEnv site site
unwrapKCRHE RunHandlerEnv{..}
  = RunHandlerEnv
      { rheRender = rheRender . KCRoute
      , rheRoute = unKCRoute <$> rheRoute
      , rheRouteToMaster = unKCRoute . rheRouteToMaster . KCRoute
      , rheSite = unKatipContextSite rheSite
      , rheChild = unKatipContextSite rheSite
      , ..
      }

wrapKC :: HandlerFor site a -> HandlerFor (KatipContextSite loggingApproach site) a
wrapKC (HandlerFor innerHandler) = HandlerFor \(HandlerData{..}) ->
  innerHandler $ HandlerData
    { handlerEnv = unwrapKCRHE handlerEnv
    , ..
    }

unwrapKC :: HandlerFor (KatipContextSite loggingApproach site) a
        -> HandlerFor site a
unwrapKC (HandlerFor outerHandler) = HandlerFor \(HandlerData{..}) ->
  outerHandler $ HandlerData
    { handlerEnv = wrapKCRHE handlerEnv
    , ..
    }

insideKC :: (HandlerFor site a -> HandlerFor site b)
        -> HandlerFor (KatipContextSite loggingApproach site) a
        -> HandlerFor (KatipContextSite loggingApproach site) b
insideKC handler
  = wrapKC . handler . unwrapKC

wrapKCW :: WidgetFor site a -> WidgetFor (KatipContextSite loggingApproach site) a
wrapKCW (WidgetFor innerWidget)
  -- TODO: figure out how best to do this. 'tellWidget' looks like the
  -- right approach, but its semantics need checking
  = error "This has not yet been implemented!"

unwrapKCW :: WidgetFor (KatipContextSite loggingApproach site) a -> WidgetFor site a
unwrapKCW (WidgetFor outerWidget)
  -- TODO: ditto
  = error "This has not yet been implemented!"

instance (Has a site) => (Has a (KatipContextSite loggingApproach site)) where
  getter  = getter . unKatipContextSite
  modifier mod = (modifier mod <$>)
  hasLens f = (KatipContextSite <$>) . hasLens f . unKatipContextSite

instance (RenderRoute site, Eq (Route site)) => RenderRoute (KatipContextSite loggingApproach site) where
  data Route (KatipContextSite loggingApproach site) = KCRoute { unKCRoute :: Route site }
  renderRoute (KCRoute route) = renderRoute @site route

deriving instance Eq (Route site) => Eq (Route (KatipContextSite loggingApproach site))

instance (Yesod site, Eq (Route site), Has LogEnv site, Has LogContexts site, Has Namespace site, SingLoggingApproach loggingApproach)
          => Yesod (KatipContextSite loggingApproach site) where
  approot
    = case approot @site of
        ApprootRelative -> ApprootRelative
        ApprootStatic root -> ApprootStatic root
        ApprootMaster f -> ApprootMaster (f . unKatipContextSite)
        ApprootRequest f -> ApprootRequest (f . unKatipContextSite)
        -- Because new cases may be added without a major bump, we need a
        -- fallback case for version compatibility
        _ -> guessApproot

  errorHandler err = wrapKC (errorHandler err)

  defaultLayout = wrapKC . defaultLayout . unwrapKCW

  urlParamRenderOverride (KatipContextSite site) (KCRoute route) query
    = urlParamRenderOverride site route query

  isAuthorized (KCRoute route) = wrapKC . isAuthorized route

  isWriteRequest = wrapKC . isWriteRequest . unKCRoute

  authRoute (KatipContextSite site)
    = KCRoute <$> authRoute site

  cleanPath = cleanPath . unKatipContextSite

  joinPath = joinPath . unKatipContextSite

  addStaticContent ext mime content
    = ((first KCRoute <$>) <$>) <$> wrapKC (addStaticContent ext mime content)

  maximumContentLength (KatipContextSite site) mRoute
    = maximumContentLength site (unKCRoute <$> mRoute)

  maximumContentLengthIO (KatipContextSite site) mRoute
    = maximumContentLengthIO site (unKCRoute <$> mRoute)

  makeLogger = makeLogger . unKatipContextSite

  messageLoggerSource (KatipContextSite site) logger loc source level str = do
    shouldLog <- shouldLogIO site source level

    let logYesod = messageLoggerSource site logger loc source level str
        logKatip = do
          guard shouldLog
          katipLogWithContexts (getter site) (getter site) (getter site) loc source level str
        loggingApproach = fromSLoggingApproach (singLoggingApproach @loggingApproach)

    case loggingApproach of
      KatipOnly ->
        logKatip

      YesodOnly ->
        logYesod

      Both -> do
        logKatip
        logYesod

  jsLoader (KatipContextSite site)
    = case jsLoader site of
        BottomOfBody -> BottomOfBody
        BottomOfHeadBlocking -> BottomOfHeadBlocking
        BottomOfHeadAsync asyncLoader ->
          BottomOfHeadAsync \urls mAsyncWidget widgetToMake ->
            let mInnerWidget = (\generator renderer -> generator (renderer . unKCRoute)) <$> mAsyncWidget
            in
              asyncLoader urls mInnerWidget (widgetToMake . KCRoute)

  jsAttributes = jsAttributes . unKatipContextSite

  jsAttributesHandler = wrapKC jsAttributesHandler

  makeSessionBackend = makeSessionBackend . unKatipContextSite

  fileUpload = fileUpload . unKatipContextSite

  shouldLogIO = shouldLogIO . unKatipContextSite

  yesodMiddleware = insideKC yesodMiddleware

  yesodWithInternalState (KatipContextSite site) mRoute = yesodWithInternalState site (unKCRoute <$> mRoute)

  defaultMessageWidget snippet widget
    = wrapKCW (defaultMessageWidget snippet (\renderer -> widget (renderer . unKCRoute)))
