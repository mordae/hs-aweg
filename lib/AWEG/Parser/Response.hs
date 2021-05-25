-- |
-- Module      :  AWEG.Parser.Response
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module AWEG.Parser.Response
  ( Response(..)
  , pResponse
  )
where
  import Praha hiding (many)

  import Text.ParserCombinators.ReadP
  import Numeric


  data Response
    = Response
      { code           :: Int
      , status         :: String
      , parts          :: [Int64]
      , limitInfo      :: Maybe Int64
      , featureInfo    :: Maybe [(String, String)]
      , waitFor        :: Maybe Int64
      , info           :: [(Int, String)]
      }
    deriving (Show)


  pResponse :: ReadP Response
  pResponse = do
    stline <- pStatusLine <* char '\n'
    infos  <- endBy pInfoLine (char '\n')

    let info = flip mapMaybe infos \case
          Info n v   -> Just (n, v)
          _otherwise -> Nothing

    let limitInfo = search infos \case
          DailyLimit n -> Just n
          _otherwise   -> Nothing

    let featureInfo = search infos \case
          Features kvs -> Just kvs
          _otherwise   -> Nothing

    let waitFor = search infos \case
          Reconnect n -> Just n
          _otherwise  -> Nothing

    case stline of
      StatusAccepted code parts -> do
        let status = ""
         in return Response{..}

      Status code status -> do
        let parts = []
         in return Response{..}


  search :: [a] -> (a -> Maybe b) -> Maybe b
  search xs fn = listToMaybe $ mapMaybe fn xs


  data StatusLine
    = StatusAccepted Int [Int64]
    | Status Int String


  pStatusLine :: ReadP StatusLine
  pStatusLine = pStatusAccepted <++ pStatus
    where
      pStatusAccepted = StatusAccepted
                          <$> holds (\c -> c >= 200 && c < 300)
                                    (readS_to_P readDec)
                          <*> pBodyparts

      pStatus = Status <$> readS_to_P readDec <* char ' ' <*> line

      pBodyparts = do
        -- Input looks like this:
        --
        -- [1] bodypart, accepted as [hex]
        -- [3] bodyparts, accepted as [hex hex hex]
        --
        -- So skip to the second '[' and go from there.
        --
        _     <- manyTill (except "[\n") (char '[')
        _     <- manyTill (except "[\n") (char '[')
        parts <- sepBy (readS_to_P readHex) (char ' ')
        _     <- char ']'
        return parts


  data InfoLine
    = Info Int String
    | DailyLimit Int64
    | Features [(String, String)]
    | Reconnect Int64


  pInfoLine :: ReadP InfoLine
  pInfoLine = pDailyLimit <++ pFeatures <++ pReconnect <++ pInfo
    where
      pDailyLimit  = DailyLimit <$> (string "102:" *> readS_to_P readDec)
      pFeatures    = Features <$> (string "103:" *> pFeatureList)
      pReconnect   = Reconnect <$> (string "104:" *> readS_to_P readDec)

      pInfo        = Info <$> readS_to_P readDec <* char ':' <*> line

      pFeatureList = sepBy pFeatureKV (char ';')
      pFeatureKV   = (,) <$> many (except "=;\n") <* char '='
                         <*> many (except "=;\n")


  holds :: (a -> Bool) -> ReadP a -> ReadP a
  holds cond subp = do
    val <- subp
    if cond val
       then return val
       else pfail


  except :: [Char] -> ReadP Char
  except chars = satisfy (not . (`elem` chars))


  line :: ReadP String
  line = many (except "\n")


-- vim:set ft=haskell sw=2 ts=2 et:
