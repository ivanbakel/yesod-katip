{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : Yesod.Katip.Orphans
Description : Orphan instances for Katip logging in Yesod handlers
Copyright   : (c) Isaac van Bakel, 2020
License     : BSD3
Maintainer  : ivb@vanbakel.io
Stability   : experimental
Portability : POSIX

This is an instance-only module, containing two useful orphan instances for
the Yesod 'HandlerFor' monad: 'Katip', and 'KatipContext'.

These instances allow you to use Katip logging functions inside Yesod
handlers by including the necessary Katip state in your @site@ type. For
example, by adding a Katip 'LogEnv' to your site, and defining the necessary
@'Has' LogEnv site@ instance, you can then use any 'Katip' logging functions
(like 'logMsg') directly in the handler.
-}

module Yesod.Katip.Orphans () where

import Data.Has
import Katip
import Yesod.Core
import Yesod.Core.Types

import Control.Monad.Reader

instance (Has LogEnv site) => Katip (HandlerFor site) where
  getLogEnv = HandlerFor (pure . getter . rheSite . handlerEnv)
  localLogEnv localLE 
    = local \handlerData ->
        let newSite = modifier localLE $ rheSite $ handlerEnv handlerData
            newHandlerEnv = (handlerEnv handlerData) { rheSite = newSite }
        in handlerData { handlerEnv = newHandlerEnv }

instance (Katip (HandlerFor site), Has LogContexts site, Has Namespace site) => KatipContext (HandlerFor site) where
  getKatipContext = HandlerFor (pure . getter . rheSite . handlerEnv)
  localKatipContext localKC
    = local \handlerData ->
        let newSite = modifier localKC $ rheSite $ handlerEnv handlerData
            newHandlerEnv = (handlerEnv handlerData) { rheSite = newSite }
        in handlerData { handlerEnv = newHandlerEnv }
  getKatipNamespace = HandlerFor (pure . getter . rheSite . handlerEnv)
  localKatipNamespace localKN 
    = local \handlerData ->
        let newSite = modifier localKN $ rheSite $ handlerEnv handlerData
            newHandlerEnv = (handlerEnv handlerData) { rheSite = newSite }
        in handlerData { handlerEnv = newHandlerEnv }
