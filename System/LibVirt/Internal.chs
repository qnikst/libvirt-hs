{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, DeriveDataTypeable #-}

{# context lib="virt" prefix="vir" #}

#include <libvirt/libvirt.h>

-- | Internal types definitions and low-level functions.
-- This module is not supposed to be imported by client code.
module System.LibVirt.Internal
  (Connection (..), Domain (..), Network (..), Interface(..)
  ) where

import Data.Generics
import Foreign
import Foreign.C.Types
import Foreign.C.String

{# pointer *virConnectPtr as Connection newtype #}

deriving instance Eq Connection
deriving instance Data Connection
deriving instance Typeable Connection

instance Show Connection where
  show (Connection ptr) = "<Connection: " ++ show ptr ++ ">"

{# pointer *virDomainPtr as Domain newtype #}

deriving instance Eq Domain
deriving instance Data Domain
deriving instance Typeable Domain

instance Show Domain where
  show (Domain ptr) = "<Domain: " ++ show ptr ++ ">"

{# pointer *virNetworkPtr as Network newtype #}

deriving instance Eq Network
deriving instance Data Network
deriving instance Typeable Network

instance Show Network where
  show (Network ptr) = "<Network: " ++ show ptr ++ ">"

{# pointer *virInterfacePtr as Interface newtype #}

deriving instance Eq Interface
deriving instance Data Interface
deriving instance Typeable Interface
