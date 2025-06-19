{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_hostname (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "hostname"
version :: Version
version = Version [1,0] []

synopsis :: String
synopsis = "A very simple package providing a cross-platform means of determining the hostname"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
