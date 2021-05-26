-- |
-- Module      :  AWEG.Types
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module AWEG.Types
  ( PhoneNumber
  , parsePhoneNumber
  , OutgoingSMS(..)
  , SendResult(..)
  , News(..)
  , Receipt(..)
  , IncomingSMS(..)
  , PartStatus(..)
  )
where
  import Praha

  import AWEG.Parser.PhoneNumber

  import Data.Time (LocalTime)


  -- |
  -- A short message we want to send.
  --
  data OutgoingSMS
    = OutgoingSMS
      { recipient      :: PhoneNumber
        -- ^ Who to send the message to.
      , payload        :: Text
        -- ^ What should the message contain. Preferably non-empty.
      , bulkId         :: Maybe Int64
        -- ^ Optional bulk identifier when sending the same thing
        --   to a whole bunch of numbers.
      , application    :: Text
        -- ^ Front-end application identifier.
      , needReceipt    :: Bool
        -- ^ Whether to ask for message delivery receipt.
      }
    deriving (Show)


  -- |
  -- Immediate result of our attempt to send a message.
  --
  data SendResult
    = MsgAccepted
      { parts          :: [Int64]
        -- ^ Identifiers of the parts the message was split into.
        --   Can be used for tracking when asked for delivery receipt.
      }
    | MsgRejected
      { reason         :: Text
        -- ^ Reason for the rejection.
      }
    | Retry
      { reason         :: Text
        -- ^ Reason for temporary rejection.
      }
    deriving (Show)


  -- |
  -- Sum type with whatever news the gateway sends back to us.
  --
  data News
    = MessageReceived
      { message        :: IncomingSMS
        -- ^ Message we have received.
      }
    | DeliveryUpdate
      { receipt        :: Receipt
        -- ^ Delivery receipt wa have received.
      }
    deriving (Show)


  -- |
  -- A message delivery receipt.
  --
  data Receipt
    = Receipt
      { part           :: Int64
        -- ^ What message part is this about. For acknowledgement.
      , bulk           :: Int64
        -- ^ Bulk identifier, random unless we specified our own.
      , status         :: PartStatus
        -- ^ Success, failure?
      , ts             :: LocalTime
        -- ^ Timestamp of the event.
      , number         :: PhoneNumber
        -- ^ The number we were trying to reach.
      }
    deriving (Show)


  -- |
  -- A message we have received.
  --
  -- WARNING: Receiving messages was not tested at all. Patches welcome!
  --
  data IncomingSMS
    = IncomingSMS
      { part           :: Int64
        -- ^ Just a unique identifier for acknowledgement.
      , sender         :: PhoneNumber
        -- ^ Number that has sent the message.
      , ts             :: LocalTime
        -- ^ Timestamp. Not sure what exactly.
      , payload        :: Text
        -- ^ The actual text of the message.
      }
    deriving (Show)


  -- |
  -- Part of the delivery receipt: status of the message.
  --
  data PartStatus
    = Unknown
    | Enroute
    | Delivered
    | Expired
    | Deleted
    | Undeliverable
    | Accepted
    | Invalid
    | Rejected
    deriving (Show)


-- vim:set ft=haskell sw=2 ts=2 et:
