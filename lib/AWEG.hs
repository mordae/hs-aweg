-- |
-- Module      :  AWEG
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--
-- This module provides routines for interaction with AWEG SMS gateway.
--
-- Coded to the specification for version 2.01 as described in
-- <https://www.t-mobile.cz/dcpublic/22985_TM_AWEG-HTTP-OpenInterface_201.pdf>.
--

module AWEG
  ( module AWEG.Types
  , Gateway
  , makeGateway

    -- * Sending Messages
  , sendSMS

    -- * Receiving Messages and Receipts
  , watchNews
  , ackMessage
  , ackReceipt

    -- * Miscelanoues
  , getDailyLimitRest
  )
where
  import Praha
  import Praha.Logger

  import AWEG.Build
  import AWEG.Parser.Report
  import AWEG.Parser.Response
  import AWEG.Types

  import Data.Conduit as Conduit
  import Data.Conduit.Combinators as Conduit

  import Network.HTTP.Client (responseTimeoutMicro)
  import Network.HTTP.Simple hiding (Response)

  import Data.Time.Clock.System (SystemTime(..), getSystemTime)

  import UnliftIO.Concurrent (threadDelay)
  import UnliftIO.Exception (catch)
  import UnliftIO.IORef

  import Text.ParserCombinators.ReadP (readP_to_S, eof)
  import Numeric (showHex)


  release :: Text
  release = "hs-aweg/" <> cs packageVersion


  -- | Tag for logging.
  tag :: LogStr
  tag = "aweg"


  -- |
  -- Wait for 300 seconds (5 minutes) after we've been told off by default.
  -- This should only apply to the situation when our quota was exceeded.
  --
  defaultWait :: Int64
  defaultWait = 300


  -- |
  -- Context for communication with the SMS gateway.
  --
  -- Retains information about timeouts as well as some miscelaneous
  -- information gathered from the replies.
  --
  data Gateway
    = Gateway
      { baseRequest    :: Request
      , sendAfter      :: IORef (Maybe Int64)
      , dailyLimitRest :: IORef (Maybe Int64)
      , featureList    :: IORef (Maybe [(String, String)])
      , anumber        :: IORef Text
      , pollSleep      :: IORef Int64
      , pollPause      :: IORef Int64
      , pollBackoff    :: IORef Int64
      , pollAfter      :: IORef (Maybe Int64)
      }


  -- |
  -- Get last known remaining daily quota.
  --
  getDailyLimitRest :: (MonadIO m) => Gateway -> m (Maybe Int64)
  getDailyLimitRest Gateway{dailyLimitRest} = readIORef dailyLimitRest


  -- |
  -- Create gateway context using:
  --
  -- Example:
  --
  -- @
  -- gw <- 'makeGateway' \"https:\/\/aweg.t-mobile.cz\/\" \"scully\" \"letme1n\"
  -- @
  --
  makeGateway :: (MonadIO m) => Text -> Text -> Text -> m Gateway
  makeGateway baseurl login password = do
    baseRequest    <- pure $ fromString (cs baseurl)
                           & addParam "auth" (login <> ":" <> password)

    sendAfter      <- newIORef Nothing
    dailyLimitRest <- newIORef Nothing
    featureList    <- newIORef Nothing

    anumber        <- newIORef ""
    pollSleep      <- newIORef 290
    pollPause      <- newIORef 1
    pollBackoff    <- newIORef 300
    pollAfter      <- newIORef Nothing

    return Gateway{..}


  -- |
  -- Send SMS through the gateway.
  --
  -- NOTE: Documentation states that messages can be rejected for various
  -- reasons, such as trying to send a \"premium SMS\" without them
  -- being enabled, but it accepts basically anything and then just loses it.
  -- The only thing that gets rejected reliably is empty payload.
  --
  sendSMS :: (MonadLogger m, MonadUnliftIO m)
          => Gateway -> OutgoingSMS -> m SendResult
  sendSMS Gateway{..} sms@OutgoingSMS{..} = do
    -- Make sure we do not call the gateway too often.
    tryWaiting sendAfter

    -- Then issue the request.
    logDebug tag ["Send ", toLogStr (show sms)]
    Response{..} <- issueRequest $ baseRequest
                                 & addParam "receiver" (cs recipient)
                                 & addParam "smstext" (payload)
                                 & addParam "use_anumber" "1"
                                 & addParamIf needReceipt "report" "1"
                                 & addParamMaybe "bulk" (fmap (cs . show) bulkId)
                                 & addParam "be" release
                                 & addParam "fe" application

    -- Update miscelanoues information from the gateway.
    modifyIORef dailyLimitRest (limitInfo <|>)
    modifyIORef featureList (featureInfo <|>)

    let result = do
          case code of
            -- All 2xx codes mean the message has been accepted.
            _ | code >= 200 && code < 300  -> MsgAccepted parts

            -- We are over our daily quota.
            302 -> Retry (cs status)

            -- Too many connections, try again later.
            309 -> Retry (cs status)

            -- This is not an actual response code, but our synthetic code
            -- saying we have failed to reach the server. Retry as per spec.
            999 -> Retry (cs status)

            _otherwise -> MsgRejected (cs status)

    case result of
      MsgAccepted{} -> do
        logInfo tag ["Message accepted, parts ", toLogStr (show parts), "."]
        return result

      MsgRejected{} -> do
        logWarning tag ["Message rejected: ", toLogStr (show status)]
        return result

      Retry{} -> do
        logInfo tag ["Will retry: ", toLogStr (show status)]
        setDelay sendAfter (maybe defaultWait id waitFor)
        return result


  -- |
  -- Wait until system time reaches the second stored in the given IORef.
  -- Then reset the IORef back to Nothing.
  --
  tryWaiting :: (MonadIO m) => IORef (Maybe Int64) -> m ()
  tryWaiting var = do
    maybeWait <- readIORef var

    case maybeWait of
      Just until' -> do
        now <- getSystemSeconds

        when (until' > now) do
          threadDelay (1000000 * (fromIntegral $ until' - now))

        writeIORef var Nothing

      Nothing -> return ()


  -- |
  -- Long-poll the gateway in order to receive 'News' indefinitely.
  --
  -- Make sure to properly 'ackMessage' and 'ackReceipt'.
  --
  watchNews :: (MonadLogger m, MonadUnliftIO m)
            => Gateway -> (News -> m ()) -> m ()
  watchNews gw@Gateway{..} handler = forever do
    catch
      do
        tryWaiting pollAfter

        logDebug tag ["Starting long poll..."]
        sleep <- readIORef pollSleep
        longPoll gw handler $ baseRequest
                            & setRequestPath "/longtime"
                            & addParam "be" release
                            & addParam "sleep" (cs (show sleep))
                            & setRequestResponseTimeout
                                (responseTimeoutMicro 3600_000_000)

        delay <- readIORef pollPause
        setDelay pollAfter delay

      \(ex :: HttpException) -> do
        logError tag ["Caught ", toLogStr (show ex)]
        delay <- readIORef pollBackoff
        logInfo tag ["Next poll delayed by ", toLogStr delay, " seconds."]
        setDelay pollAfter delay


  longPoll :: (MonadLogger m, MonadUnliftIO m)
           => Gateway -> (News -> m ()) -> Request -> m ()
  longPoll gw handler request = do
    withResponse request \resp -> do
      runConduit do
        getResponseBody resp
          .| linesUnboundedAscii
          .| decodeUtf8Lenient
          .| Conduit.mapM_ (parseAndHandle gw handler)


  parseAndHandle :: (MonadLogger m)
                 => Gateway -> (News -> m ()) -> Text -> m ()
  parseAndHandle Gateway{..} handler text = do
    case readP_to_S (pReportLine <* eof) (cs text) of
      (RepStatus code, ""):_ -> do
        when (code < 200 || code >= 300) do
          logError tag ["Report failed with code ", toLogStr code]
          backoff <- readIORef pollBackoff
          setDelay pollAfter backoff

      (RepSleepTime secs, ""):_   -> writeIORef pollSleep secs
      (RepPauseTime secs, ""):_   -> writeIORef pollPause secs
      (RepBackoffTime secs, ""):_ -> writeIORef pollBackoff secs
      (RepAnumber anumber', ""):_ -> writeIORef anumber anumber'

      (RepMessage message, ""):_ -> do
        logDebug tag [toLogStr (show message)]
        handler MessageReceived{..}

      (RepReceipt receipt, ""):_ -> do
        logDebug tag [toLogStr (show receipt)]
        handler DeliveryUpdate{..}

      _otherwise -> do
        logError tag ["Failed to parse: ", toLogStr text]


  -- |
  -- Acknowledge incoming message.
  --
  -- You should really do this for every received message.
  -- Otherwise the gateway will keep pestering us about them
  -- ofter couple of seconds.
  --
  ackMessage :: (MonadLogger m, MonadUnliftIO m)
             => Gateway -> Int64 -> m ()
  ackMessage Gateway{..} part = do
    logDebug tag ["ACK message, part ", toLogStr part, "."]
    issueRequest_ $ baseRequest
                  & setRequestPath "/longtime"
                  & addParam "ack" (cs ("M:" <> showHex part ""))
                  & addParam "be" release


  -- |
  -- Acknowledge message delivery receipt.
  --
  -- Ditto, please use this.
  --
  ackReceipt :: (MonadLogger m, MonadUnliftIO m)
             => Gateway -> Int64 -> m ()
  ackReceipt Gateway{..} part = do
    logDebug tag ["ACK receipt, part ", toLogStr part, "."]
    issueRequest_ $ baseRequest
                  & setRequestPath "/longtime"
                  & addParam "ack" (cs ("R:" <> showHex part ""))
                  & addParam "be" release


  issueRequest_ :: (MonadLogger m, MonadUnliftIO m) => Request -> m ()
  issueRequest_ request = void $ issueRequest request


  issueRequest :: (MonadLogger m, MonadUnliftIO m)
               => Request -> m Response
  issueRequest request = do
    catch (issueRequest' request)
          \(ex :: HttpException) -> do
            logError tag ["Caught: ", toLogStr (show ex)]
            return $ badResponse (show ex)


  issueRequest' :: (MonadLogger m)
                => Request -> m Response
  issueRequest' request = do
    resp <- httpLBS request

    case readP_to_S (pResponse <* eof) $ cs $ getResponseBody resp of
      ((gwresponse, ""):_) -> return gwresponse
      _otherwise -> return $ badResponse "failed to parse response"


  badResponse :: String -> Response
  badResponse status = Response { code        = 999
                                , status      = status
                                , parts       = []
                                , limitInfo   = Nothing
                                , featureInfo = Nothing
                                , waitFor     = Nothing
                                , info        = []
                                }


  -- Utilities ---------------------------------------------------------------


  setDelay :: (MonadIO m) => IORef (Maybe Int64) -> Int64 -> m ()
  setDelay ioref delay = do
    now <- getSystemSeconds
    writeIORef ioref (Just (now + delay))


  getSystemSeconds :: (MonadIO m) => m Int64
  getSystemSeconds = liftIO $ systemSeconds <$> getSystemTime


  addParam :: Text -> Text -> Request -> Request
  addParam name value = addToRequestQueryString [(cs name, Just (cs value))]


  addParamIf :: Bool -> Text -> Text -> Request -> Request
  addParamIf cond = if cond then addParam else \_ _ -> id


  addParamMaybe :: Text -> (Maybe Text) -> Request -> Request
  addParamMaybe name maybeValue = case maybeValue of
                                    Nothing -> id
                                    Just value -> addParam name value


-- vim:set ft=haskell sw=2 ts=2 et:
