{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Yesod.Katip.Internal.Singletons
  ( LoggingApproach (..)
  , SLoggingApproach ()
  , fromSLoggingApproach
  , SingLoggingApproach (..)
  ) where

data LoggingApproach
  = YesodOnly
  | KatipOnly
  | Both

data SLoggingApproach (k :: LoggingApproach) where
  SYesodOnly :: SLoggingApproach YesodOnly
  SKatipOnly :: SLoggingApproach KatipOnly
  SBoth      :: SLoggingApproach Both

fromSLoggingApproach :: SLoggingApproach k -> LoggingApproach
fromSLoggingApproach SYesodOnly = YesodOnly
fromSLoggingApproach SKatipOnly = KatipOnly
fromSLoggingApproach SBoth      = Both

class SingLoggingApproach (k :: LoggingApproach) where
  singLoggingApproach :: SLoggingApproach k

instance SingLoggingApproach YesodOnly where
  singLoggingApproach = SYesodOnly

instance SingLoggingApproach KatipOnly where
  singLoggingApproach = SKatipOnly

instance SingLoggingApproach Both where
  singLoggingApproach = SBoth
