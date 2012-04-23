{-# LANGUAGE ForeignFunctionInterface, StandaloneDeriving, DeriveDataTypeable, EmptyDataDecls #-}

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
   getDomainID, getDomainName,

   -- * Domains control
   createDomain, createDomainXML,
   destroyDomain, 
   shutdownDomain, rebootDomain,
   suspendDomain, resumeDomain,
   saveDomain, restoreDomain,
   refDomain, freeDomain,

   -- * Networks management
   getNetworkConnection,
   runningNetworksCount, definedNetworksCount,
   runningNetworksNames, definedNetworksNames,
   lookupNetworkName, lookupNetworkUUID,
   createNetworkXML, defineNetworkXML,
   undefineNetwork, destroyNetwork,
   createNetwork,
   refNetwork, freeNetwork,
   getNetworkName,
   getNetworkXML
  ) where

import Data.Bits
import Data.Generics
import Foreign
import Foreign.C.Types
import Foreign.C.String

{# import System.LibVirt.Internal #}
{# import System.LibVirt.Errors #}

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

data NetworkXMLFlags = NetworkXML
  deriving (Eq, Show, Enum)

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

{# fun virInitialize as initialize { } -> `Int' exceptionOnMinusOne* #}

{# fun virConnectOpen as openConnection
    { `String' } -> `Connection' ptrToConnection* #}

{# fun virConnectClose as closeConnection
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

{# fun virConnectNumOfDomains as runningDomainsCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

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
      id              `DomainID'    } -> `Domain' ptrToDomain* #}

{# fun virDomainGetID as getDomainID
    { domainToPtr `Domain' } -> `DomainID' cIntConv #}

{# fun virDomainGetName as getDomainName
    { domainToPtr `Domain' } -> `String' #}

{# fun virDomainLookupByName as lookupDomainName
    { connectionToPtr `Connection',
                      `String'      } -> `Domain' ptrToDomain* #}

{# fun virConnectNumOfDefinedDomains as definedDomainsCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

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
                      `String'      } -> `Domain' ptrToDomain* #}

{# fun virDomainUndefine as undefineDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainCreate as createDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainCreateXML as createDomainXML
    { connectionToPtr `Connection',
                      `String',
      flags2int       `[DomainCreateFlags]' } -> `Domain' ptrToDomain* #}

{# fun virDomainGetXMLDesc as getDomainXML
    { domainToPtr `Domain',
      flags2int   `[DomainXMLFlags]' } -> `String' #}

{# fun virDomainShutdown as shutdownDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainReboot as rebootDomain
    { domainToPtr `Domain',
      id          `CUInt'  } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainDestroy as destroyDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainRef as refDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainFree as freeDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainSuspend as suspendDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainResume as resumeDomain
    { domainToPtr `Domain' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainSave as saveDomain
    { domainToPtr `Domain',
                  `String' } -> `Int' exceptionOnMinusOne* #}

{# fun virDomainRestore as restoreDomain
    { connectionToPtr `Connection',
                      `String'      } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkGetConnect as getNetworkConnection
    { networkToPtr `Network' } -> `Connection' ptrToConnection* #}

{# fun virConnectNumOfNetworks as runningNetworksCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

runningNetworksNames :: Connection -> IO [String]
runningNetworksNames conn = do
  cn <- {# call virConnectNumOfNetworks #} (connectionToPtr conn)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virConnectListNetworks #} (connectionToPtr conn) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virConnectNumOfDefinedNetworks as definedNetworksCount
    { connectionToPtr `Connection' } -> `Int' exceptionOnMinusOne* #}

definedNetworksNames :: Connection -> IO [String]
definedNetworksNames conn = do
  cn <- {# call virConnectNumOfDefinedNetworks #} (connectionToPtr conn)
  let n = fromIntegral cn
  allocaArray n $ \nptr -> do
    {# call virConnectListDefinedNetworks #} (connectionToPtr conn) nptr cn
    mapM peekCString =<< peekArray n nptr

{# fun virNetworkLookupByName as lookupNetworkName
    { connectionToPtr `Connection', `String' } -> `Network' ptrToNetwork* #}

withCUString :: String -> (Ptr CUChar -> IO a) -> IO a
withCUString str fn = withCString str (fn . castPtr)

{# fun virNetworkLookupByUUID as lookupNetworkUUID
    { connectionToPtr `Connection', withCUString* `String' } -> `Network' ptrToNetwork* #}

{# fun virNetworkCreateXML as createNetworkXML
    { connectionToPtr `Connection', `String' } -> `Network' ptrToNetwork* #}

{# fun virNetworkDefineXML as defineNetworkXML
    { connectionToPtr `Connection', `String' } -> `Network' ptrToNetwork* #}

{# fun virNetworkUndefine as undefineNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkCreate as createNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkDestroy as destroyNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkRef as refNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkFree as freeNetwork
    { networkToPtr `Network' } -> `Int' exceptionOnMinusOne* #}

{# fun virNetworkGetName as getNetworkName
    { networkToPtr `Network' } -> `String' #}

{# fun virNetworkGetXMLDesc as getNetworkXML
    { networkToPtr `Network', 
      flags2int    `[NetworkXMLFlags]' } -> `String' #}

