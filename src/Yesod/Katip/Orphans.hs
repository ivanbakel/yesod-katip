{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE UndecidableInstances #-}

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
