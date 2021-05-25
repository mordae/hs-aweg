-- |
-- Module      :  AWEG.Build
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

{-# LANGUAGE CPP #-}

module AWEG.Build
  ( packageVersion
  )
where
  import Praha


  packageVersion :: String
  packageVersion = CURRENT_PACKAGE_VERSION


-- vim:set ft=haskell sw=2 ts=2 et:
