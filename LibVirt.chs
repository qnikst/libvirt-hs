{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="virt" prefix="vir" #}

#include <libvirt/libvirt.h>

module LibVirt where

import Foreign
import Foreign.C.Types
import Foreign.C.String

cIntConv = fromIntegral

{# pointer *virConnectPtr as Connection newtype #}

ptrToConnection :: Ptr () -> Connection
ptrToConnection ptr = Connection (castPtr ptr)

connectionToPtr :: Connection -> Ptr ()
connectionToPtr (Connection ptr) = castPtr ptr

{# pointer *virDomainPtr as Domain newtype #}

ptrToDomain :: Ptr () -> Domain
ptrToDomain ptr = Domain (castPtr ptr)

domainToPtr :: Domain -> Ptr ()
domainToPtr (Domain ptr) = castPtr ptr

data DomainInfo = DomainInfo {
  diState :: CUChar,
  diMaxMem :: CULong,
  diMemory :: CULong,
  diNrVirtCPU :: CUShort,
  diCPUTime :: CULLong }
  deriving (Eq, Show)

{# pointer *virDomainInfo as DomainInfoPtr -> DomainInfo #}

{# enum DomainState {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainCreateFlags {underscoreToCase} deriving (Eq, Show) #}

{# pointer *virStreamPtr as Steram newtype #}

data SecurityLabel = SecurityLabel {
  slLabel :: String,
  slEnforcing :: Int }
  deriving (Eq, Show)

{# pointer *virSecurityLabelPtr as SecurityLabelPtr -> SecurityLabel #}

data SecurityModel = SecurityModel {
  smModel :: String,
  smDOI :: String }
  deriving (Eq, Show)

{# pointer *virSecurityModelPtr as SecurityModelPtr -> SecurityModel #}

data NodeInfo = NodeInfo {
  niModel :: String,
  niMemory :: CULong,
  niCPUs :: CUInt,
  niMHz :: CUInt,
  niNodes :: CUInt,
  niSockets :: CUInt,
  niCores :: CUInt,
  niThreads :: CUInt }
  deriving (Eq, Show)

{# pointer *virNodeInfoPtr as NodeInfoPtr -> NodeInfo #}

{# enum SchedParameterType {underscoreToCase} deriving (Eq, Show) #}

data ConnectCredential = ConnectCredential {
  ccType :: Int,
  ccPrompt :: String,
  ccChallenge :: String,
  ccDefresult :: String,
  ccResult :: String,
  ccResultLen :: Integer }
  deriving (Eq, Show)

{# pointer *virConnectCredentialPtr as ConnectCredentialPtr -> ConnectCredential #}

{# fun virInitialize as initialize { } -> `Int' #}

{# fun virConnectOpen as openConnection { `String' } -> `Connection' ptrToConnection #}

{# fun virConnectClose as closeConnection { connectionToPtr `Connection' } -> `Int' #}

{# fun virConnectNumOfDomains as domainsCount { connectionToPtr `Connection' } -> `Int' #}

type DomainID = CInt

domainsIDs :: Connection -> IO [DomainID]
domainsIDs conn = do
  cn <- {# call virConnectNumOfDomains #} (connectionToPtr conn)
  let n = fromIntegral cn
  r <- allocaArray n $ \arr -> do
          r <- {# call virConnectListDomains #}
                  (connectionToPtr conn) arr cn
          peekArray n arr
  return r

{# fun virDomainLookupByID as lookupDomainID { connectionToPtr `Connection', id `DomainID' } -> `Domain' ptrToDomain #}

{# fun virDomainGetInfo as getDomainInfo { domainToPtr `Domain', alloca- `DomainInfoPtr' } -> `Int' #}

getDomainInfo :: Domain -> IO DomainInfo
getDomainInfo (Domain dptr) = do


