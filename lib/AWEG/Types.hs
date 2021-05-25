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
  ( CellNumber(..)
  , OutgoingSMS(..)
  , SendResult(..)
  , News(..)
  , Receipt(..)
  , IncomingSMS(..)
  , PartStatus(..)
  )
where
  import Praha

  import Data.Char (isDigit)
  import Text.Read (readsPrec)
  import Text.ParserCombinators.ReadP

  import Data.Time (LocalTime)


  newtype CellNumber
    = CellNumber
      { cellNumberText :: Text
      }
    deriving (Show)

  instance IsString CellNumber where
    fromString = CellNumber . cs

  instance ConvertibleStrings CellNumber Text where
    convertString CellNumber{..} = cellNumberText

  instance Read CellNumber where
    readsPrec _ = readP_to_S pCellNumber


  pCellNumber :: ReadP CellNumber
  pCellNumber = do
    _    <- string "+"
    nums <- many1 (satisfy isDigit)

    if length nums > 16
       then pfail
       else return $ CellNumber (cs ('+' : nums))


  data OutgoingSMS
    = OutgoingSMS
      { recipient      :: CellNumber
      , payload        :: Text
      , bulkId         :: Maybe Int64
      , application    :: Text
      , needReceipt    :: Bool
      }
    deriving (Show)


  data SendResult
    = MsgAccepted
      { parts          :: [Int64]
      }
    | MsgRejected
      { reason         :: Text
      }
    | Retry
      { reason         :: Text
      }
    deriving (Show)


  data News
    = MessageReceived
      { message        :: IncomingSMS
      }
    | DeliveryUpdate
      { receipt        :: Receipt
      }
    deriving (Show)


  data Receipt
    = Receipt
      { part           :: Int64
      , bulk           :: Int64
      , status         :: PartStatus
      , ts             :: LocalTime
      , number         :: CellNumber
      }
    deriving (Show)


  data IncomingSMS
    = IncomingSMS
      { part           :: Int64
      , sender         :: CellNumber
      , ts             :: LocalTime
      , payload        :: Text
      }
    deriving (Show)


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
