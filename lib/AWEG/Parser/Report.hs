-- |
-- Module      :  AWEG.Parser.Report
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module AWEG.Parser.Report
  ( ReportLine(..)
  , pReportLine
  )
where
  import Praha hiding (many, optional)

  import AWEG.Types

  import Network.HTTP.Types (urlDecode)

  import Data.Time

  import Text.ParserCombinators.ReadP
  import Text.Read (reads)
  import Numeric


  data ReportLine
    = RepStatus Int
    | RepSleepTime Int64
    | RepPauseTime Int64
    | RepBackoffTime Int64
    | RepAnumber Text
    | RepMessage IncomingSMS
    | RepReceipt Receipt
    deriving (Show)


  pReportLine :: ReadP ReportLine
  pReportLine = fmap RepReceipt pReceipt
                <++ fmap RepMessage pMessage
                <++ fmap RepStatus pStatusLine
                <++ fmap RepSleepTime pSleepTime
                <++ fmap RepPauseTime pPauseTime
                <++ fmap RepBackoffTime pBackoffTime
                <++ fmap RepAnumber pAnumber


  -- 200 OK
  pStatusLine :: ReadP Int
  pStatusLine = readS_to_P readDec <* char ' ' <* many get


  -- CONF:U_anumber=Pirati;
  pAnumber :: ReadP Text
  pAnumber = string "CONF:U_anumber=" *> (cs <$> many1 safe) <* sep


  -- CONF:intervalA=300
  pSleepTime :: ReadP Int64
  pSleepTime = string "CONF:intervalA=" *> readS_to_P readDec


  -- CONF:intervalB=1
  pPauseTime :: ReadP Int64
  pPauseTime = string "CONF:intervalB=" *> readS_to_P readDec


  -- CONF:intervalC=125
  pBackoffTime :: ReadP Int64
  pBackoffTime = string "CONF:intervalC=" *> readS_to_P readDec


  -- REPORT:<part>,<bulk>,<status>,<orig-ts>,<final-ts>,<number>
  -- REPORT:<part>,<bulk>,<status>,          <final-ts>,<number>
  pReceipt :: ReadP Receipt
  pReceipt = do
    _      <- string "REPORT:"
    part   <- readS_to_P readHex
    bulk   <- sep *> readS_to_P readDec
    status <- sep *> pStatus
    _      <- optional (sep *> pLocalTime)
    ts     <- sep *> pLocalTime
    number <- sep *> readS_to_P reads

    return Receipt{..}


  -- SM:<msgid>;<sender>;<recipient>;<timestamp>;<encoding>;<text>
  pMessage :: ReadP IncomingSMS
  pMessage = do
    _        <- string "SM:"
    part     <- readS_to_P readHex
    sender'  <- sep *> unescape (many1 safe)
    _recip   <- sep *> many safe
    ts       <- sep *> pLocalTime
    _enc     <- sep *> many safe
    payload' <- sep *> unescape (many (except "\n"))

    let sender  = CellNumber (cs sender')
        payload = cs payload'

    return IncomingSMS{..}


  pStatus :: ReadP PartStatus
  pStatus = choice [ char '0' *> pure Unknown
                   , char '1' *> pure Enroute
                   , char '2' *> pure Delivered
                   , char '3' *> pure Expired
                   , char '4' *> pure Deleted
                   , char '5' *> pure Undeliverable
                   , char '6' *> pure Accepted
                   , char '7' *> pure Invalid
                   , char '8' *> pure Rejected
                   , char '9' *> pure Unknown
                   ]


  pLocalTime :: ReadP LocalTime
  pLocalTime = readPTime False defaultTimeLocale "%0Y%m%d%H%M%S"


  except :: [Char] -> ReadP Char
  except chars = satisfy (not . (`elem` chars))


  unescape :: ReadP String -> ReadP String
  unescape p = (cs . urlDecode False . cs) <$> p


  sep :: ReadP Char
  sep = char ';' +++ char ','


  safe :: ReadP Char
  safe = except ";,\n"


-- vim:set ft=haskell sw=2 ts=2 et:
