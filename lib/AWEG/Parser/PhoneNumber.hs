-- |
-- Module      :  AWEG.Parser.PhoneNumber
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module AWEG.Parser.PhoneNumber
  ( PhoneNumber
  , pPhoneNumber
  , parsePhoneNumber
  )
where
  import Praha

  import Text.ParserCombinators.ReadP
  import Data.Char (isDigit)


  -- |
  -- Just a phone number in the international format with country code.
  -- That is, a @+@ sign followed by up to 15 numbers.
  --
  -- There is no validation when being converted to and from 'Text'.
  --
  newtype PhoneNumber
    = PhoneNumber
      { phoneNumber    :: Text
      }
    deriving (Show)

  -- | Throws an error when the format is not valid.
  instance IsString PhoneNumber where
    fromString str = case parsePhoneNumber str of
                       Just pn -> pn
                       Nothing -> error ("Invalid PhoneNumber: " <> str)

  instance ConvertibleStrings PhoneNumber Text where
    convertString PhoneNumber{..} = phoneNumber

  instance ConvertibleStrings Text PhoneNumber where
    convertString text = PhoneNumber text


  -- |
  -- Try to parse the phone number.
  --
  parsePhoneNumber :: (ConvertibleStrings a String) => a -> Maybe PhoneNumber
  parsePhoneNumber str = case readP_to_S (pPhoneNumber <* eof) (cs str) of
                           (pn, ""):_ -> Just pn
                           _otherwise -> Nothing


  pPhoneNumber :: ReadP PhoneNumber
  pPhoneNumber = do
    _    <- string "+"
    nums <- many1 (satisfy isDigit)

    if length nums > 15
       then pfail
       else return $ PhoneNumber (cs ('+' : nums))


-- vim:set ft=haskell sw=2 ts=2 et:
