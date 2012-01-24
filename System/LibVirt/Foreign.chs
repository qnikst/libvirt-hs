{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, DeriveDataTypeable #-}

{# context lib="virt" prefix="vir" #}

#include <libvirt/libvirt.h>

module System.LibVirt.Foreign
  (-- * Types
   Connection, Domain, Network,
   DomainID,
   DomainInfo (..),
   DomainState (..),
   Stream,
   DomainCreateFlags (..),
   DomainXMLFlags (..),
   SecurityLabel (..),
   SecurityModel (..),
   NodeInfo (..),
   SchedParameterType (..),
   ConnectCredential (..),

   -- * Connection management functions
   initialize,
   openConnection, closeConnection,

   -- * Domains management functions
   runningDomainsCount, definedDomainsCount,
   runningDomainsIDs, definedDomainsNames,
   lookupDomainID, lookupDomainName,
   getDomainInfo, getDomainXML,
   defineDomainXML, undefineDomain,

   -- * Domains control
   createDomain, createDomainXML,
   destroyDomain, 
   shutdownDomain, rebootDomain,
   suspendDomain, resumeDomain,
   saveDomain, restoreDomain,
   refDomain, freeDomain,

   -- * Networks management
   getNetworkConnection
  ) where

import Data.Bits
import Data.Generics
import Foreign
import Foreign.C.Types
import Foreign.C.String

{# import System.LibVirt.Internal #}

cIntConv = fromIntegral

cuchar2state :: CUChar -> DomainState
cuchar2state c = toEnum (fromIntegral c)

flags2int :: (Enum f, Num a) => [f] -> a
flags2int list = fromIntegral $ foldr (.|.) 0 (map fromEnum list)

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
{# enum DomainXMLFlags {underscoreToCase} deriving (Eq, Show) #}

{# pointer *virStreamPtr as Stream newtype #}

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

{# fun virConnectOpen as openConnection
    { `String' } -> `Connection' ptrToConnection #}

{# fun virConnectClose as closeConnection
    { connectionToPtr `Connection' } -> `Int' #}

{# fun virConnectNumOfDomains as runningDomainsCount
    { connectionToPtr `Connection' } -> `Int' #}

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

{# fun virDomainLookupByID as lookupDomainID
    { connectionToPtr `Connection',
      id              `DomainID'    } -> `Domain' ptrToDomain #}

{# fun virDomainLookupByName as lookupDomainName
    { connectionToPtr `Connection',
                      `String'      } -> `Domain' ptrToDomain #}

{# fun virConnectNumOfDefinedDomains as definedDomainsCount
    { connectionToPtr `Connection' } -> `Int' #}

getDomainInfo :: Domain -> IO DomainInfo
getDomainInfo dptr = do
  allocaBytes {# sizeof virDomainInfo #} $ \iptr -> do
         i <- {# call virDomainGetInfo #} (domainToPtr dptr) iptr
         state   <- {# get DomainInfo->state #}     iptr
         maxmem  <- {# get DomainInfo->maxMem #}    iptr
         memory  <- {# get DomainInfo->memory #}    iptr
         ncpus   <- {# get DomainInfo->nrVirtCpu #} iptr
         cputime <- {# get DomainInfo->cpuTime #}   iptr
         return $ DomainInfo {
                    diState     = cuchar2state state,
                    diMaxMem    = fromIntegral maxmem,
                    diMemory    = fromIntegral memory,
                    diNrVirtCPU = fromIntegral ncpus,
                    diCPUTime   = fromIntegral cputime }

{# fun virDomainDefineXML as defineDomainXML
    { connectionToPtr `Connection',
                      `String'      } -> `Domain' ptrToDomain #}

{# fun virDomainUndefine as undefineDomain
    { domainToPtr `Domain' } -> `Int' #}

{# fun virDomainCreate as createDomain
    { domainToPtr `Domain' } -> `Int' #}

{# fun virDomainCreateXML as createDomainXML
    { connectionToPtr `Connection',
                      `String',
      flags2int       `[DomainCreateFlags]' } -> `Domain' ptrToDomain #}

{# fun virDomainGetXMLDesc as getDomainXML
    { domainToPtr `Domain',
      flags2int   `[DomainXMLFlags]' } -> `String' #}

{# fun virDomainShutdown as shutdownDomain
    { domainToPtr `Domain' } -> `Int' #}

{# fun virDomainReboot as rebootDomain
    { domainToPtr `Domain',
      id          `CUInt'  } -> `Int' #}

{# fun virDomainDestroy as destroyDomain
    { domainToPtr `Domain' } -> `Int' #}

{# fun virDomainRef as refDomain
    { domainToPtr `Domain' } -> `Int' #}

{# fun virDomainFree as freeDomain
    { domainToPtr `Domain' } -> `Int' #}

{# fun virDomainSuspend as suspendDomain
    { domainToPtr `Domain' } -> `Int' #}

{# fun virDomainResume as resumeDomain
    { domainToPtr `Domain' } -> `Int' #}

{# fun virDomainSave as saveDomain
    { domainToPtr `Domain',
                  `String' } -> `Int' #}

{# fun virDomainRestore as restoreDomain
    { connectionToPtr `Connection',
                      `String'      } -> `Int' #}

{# fun virNetworkGetConnect as getNetworkConnection
    { networkToPtr `Network' } -> `Connection' ptrToConnection #}
