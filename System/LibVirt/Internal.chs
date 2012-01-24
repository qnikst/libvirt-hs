{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, DeriveDataTypeable #-}

{# context lib="virt" prefix="vir" #}

#include <libvirt/libvirt.h>

-- | Internal types definitions and low-level functions
module System.LibVirt.Internal
  (
   Connection, Domain, Network,
   ptrToConnection, ptrToDomain, ptrToNetwork,
   connectionToPtr, domainToPtr, networkToPtr
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

ptrToConnection :: Ptr () -> Connection
ptrToConnection ptr = Connection (castPtr ptr)

connectionToPtr :: Connection -> Ptr ()
connectionToPtr (Connection ptr) = castPtr ptr

{# pointer *virDomainPtr as Domain newtype #}

deriving instance Eq Domain
deriving instance Data Domain
deriving instance Typeable Domain

instance Show Domain where
  show (Domain ptr) = "<Domain: " ++ show ptr ++ ">"

ptrToDomain :: Ptr () -> Domain
ptrToDomain ptr = Domain (castPtr ptr)

domainToPtr :: Domain -> Ptr ()
domainToPtr (Domain ptr) = castPtr ptr

{# pointer *virNetworkPtr as Network newtype #}

deriving instance Eq Network
deriving instance Data Network
deriving instance Typeable Network

instance Show Network where
  show (Network ptr) = "<Network: " ++ show ptr ++ ">"

ptrToNetwork :: Ptr () -> Network
ptrToNetwork ptr = Network (castPtr ptr)

networkToPtr :: Network -> Ptr ()
networkToPtr (Network ptr) = castPtr ptr

