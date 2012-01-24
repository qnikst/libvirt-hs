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

cuchar2state :: CUChar -> DomainState
cuchar2state c = toEnum (fromIntegral c)

data DomainInfo = DomainInfo {
  diState :: DomainState,
  diMaxMem :: Integer,
  diMemory :: Integer,
  diNrVirtCPU :: Int,
  diCPUTime :: Integer }
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

{# fun virConnectNumOfDomains as runningDomainsCount { connectionToPtr `Connection' } -> `Int' #}

type DomainID = CInt

runningDomainsIDs :: Connection -> IO [DomainID]
runningDomainsIDs conn = do
  cn <- {# call virConnectNumOfDomains #} (connectionToPtr conn)
  let n = fromIntegral cn
  r <- allocaArray n $ \arr -> do
          r <- {# call virConnectListDomains #}
                  (connectionToPtr conn) arr cn
          peekArray n arr
  return r

definedDomainsNames :: Connection -> IO [String]
definedDomainsNames conn = do
  cn <- {# call virConnectNumOfDefinedDomains #} (connectionToPtr conn)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virConnectListDefinedDomains #}
        (connectionToPtr conn) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virDomainLookupByID as lookupDomainID { connectionToPtr `Connection', id `DomainID' } -> `Domain' ptrToDomain #}

{# fun virDomainLookupByName as lookupDomainName { connectionToPtr `Connection', `String' } -> `Domain' ptrToDomain #}

{# fun virConnectNumOfDefinedDomains as definedDomainsCount { connectionToPtr `Connection' } -> `Int' #}

getDomainInfo :: Domain -> IO DomainInfo
getDomainInfo (Domain dptr) = do
  allocaBytes {# sizeof virDomainInfo #} $ \iptr -> do
         i <- {# call virDomainGetInfo #} (castPtr dptr) iptr
         state <- {# get DomainInfo->state #} iptr
         maxmem <- {# get DomainInfo->maxMem #} iptr
         memory <- {# get DomainInfo->memory #} iptr
         ncpus <- {# get DomainInfo->nrVirtCpu #} iptr
         cputime <- {# get DomainInfo->cpuTime #} iptr
         return $ DomainInfo {
                    diState = cuchar2state state,
                    diMaxMem = fromIntegral maxmem,
                    diMemory = fromIntegral memory,
                    diNrVirtCPU = fromIntegral ncpus,
                    diCPUTime = fromIntegral cputime }


