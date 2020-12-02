{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Yesod.Katip.Internal.Singletons
  ( LoggingApproach (..)
  , SLoggingApproach ()
  , fromSLoggingApproach
  , SingLoggingApproach (..)
  ) where

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
  -- Yesod logger.

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
