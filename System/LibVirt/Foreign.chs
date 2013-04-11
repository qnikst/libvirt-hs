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
   DomainEventID (..),
   DomainEventType (..),
   DomainEventDefinedDetailType (..),
   DomainEventUndefinedDetailType (..),
   DomainEventStartedDetailType (..),
   DomainEventSuspendedDetailType (..),
   DomainEventResumedDetailType (..),
   DomainEventStoppedDetailType (..),
   DomainEventShutdownDetailType (..),
--   ConnectListAllInterfacesFlags (..),
   FreeCallback,
   ConnectDomainEventGenericCallback,
   ConnectDomainEventCallback,

   -- * Connection management functions
   initialize,
   openConnection, closeConnection,
   connectSetKeepAlive,
   connectGetCapabilities,

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
   getNetworkXML,

   -- * Interface operations
   connectNumOfDefinedInterfaces, connectNumOfInterfaces,
   -- connectListAllInterfaces, 
   interfaceCreate, interfaceDefineXML, interfaceDestroy,
   interfaceUndefine, interfaceIsActive,
   interfaceFree, interfaceGetMACString, interfaceGetName,
   interfaceGetXMLDesc, interfaceLookupByName,
   interfaceLookupByMACString,

   -- * callback management
   eventRegisterDefaultImpl,
   eventRunDefaultImpl,
   connectDomainEventRegister,
   connectDomainEventRegisterAny,
   connectDomainEventDeregisterAny,
   mkConnectDomainEventGenericCallback,
   mkConnectDomainEventCallback,
   mkFreeCallback

  ) where

import Data.Bits
import Data.Generics
import Data.ByteString (ByteString, packCString)
import qualified Data.ByteString as S
import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String
import Control.Monad (void)

{# import System.LibVirt.Internal #}
{# import System.LibVirt.Errors #}

cIntConv = fromIntegral

cuchar2state :: CUChar -> DomainState
cuchar2state c = toEnum (fromIntegral c)

flags2int :: (Enum f, Num a) => [f] -> a
flags2int list = fromIntegral $ foldr (.|.) 0 (map fromEnum list)

flag2int :: (Enum f, Num a) => f -> a
flag2int = fromIntegral . fromEnum

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
{# enum DomainEventID {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventDefinedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventUndefinedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventStartedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventSuspendedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventResumedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventStoppedDetailType {underscoreToCase} deriving (Eq, Show) #}
{# enum DomainEventShutdownDetailType {underscoreToCase} deriving (Eq, Show) #}
-- {# enum ConnectListAllInterfacesFlags {underscoreToCase} deriving (Eq, Show) #}


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

{# fun virConnectDomainEventRegisterAny as connectDomainEventRegisterAny
    `(Storable a)' =>
    { connectionToPtr `Connection',
      domainToPtr `Domain',
      flag2int `DomainEventID',
      castFunPtr `FunPtr (ConnectDomainEventCallback a)',
      castPtr `Ptr a',
      id `FunPtr FreeCallback'
      } -> `Int' exceptionOnMinusOne* #}

{# fun virConnectDomainEventDeregisterAny as connectDomainEventDeregisterAny
    { connectionToPtr `Connection',
      fromIntegral `Int'
    } -> `Int' exceptionOnMinusOne* #}

{# fun virEventRegisterDefaultImpl as eventRegisterDefaultImpl
     { } -> `Int'exceptionOnMinusOne* #}

{# fun virConnectSetKeepAlive as connectSetKeepAlive
      { connectionToPtr `Connection',
        id     `CInt',
        id     `CUInt'
      } -> `Int' exceptionOnMinusOne* #}

{# fun virConnectDomainEventRegister as connectDomainEventRegister
      { connectionToPtr `Connection',
        castFunPtr `FunPtr (ConnectDomainEventCallback a)',
        castPtr `Ptr a',
        id `FunPtr FreeCallback'
      } -> `Int' exceptionOnMinusOne* #}

{# fun virEventRunDefaultImpl as eventRunDefaultImpl
      {} -> `Int' exceptionOnMinusOne* #}

-- | Provides capabilities of the hypervisor / driver.
{# fun virConnectGetCapabilities as connectGetCapabilities
      { connectionToPtr `Connection' } -> `String' #}

type ConnectDomainEventGenericCallback a = Connection -> Domain -> Ptr a -> IO ()
type FreeCallback = Ptr () -> IO ()


type ConnectDomainEventCallback a = Connection -> Domain -> Int -> Int -> Ptr a -> IO ()

foreign import ccall "wrapper"
    mkConnectDomainEventCallback :: ConnectDomainEventCallback a
                                 -> IO (FunPtr (ConnectDomainEventCallback a))

foreign import ccall "wrapper"
  mkConnectDomainEventGenericCallback :: ConnectDomainEventGenericCallback a
                                      -> IO (FunPtr (ConnectDomainEventGenericCallback a))

foreign import ccall "wrapper"
  mkFreeCallback :: FreeCallback -> IO (FunPtr FreeCallback)

-- Interface operations

-- | Collect the list of interfaces, and allocate an array to store those objects. 
-- This API solves the race inherent between 'virConnectListInterfaces' and 
-- 'virConnectListDefinedInterfaces'.
-- 
-- Normally, all interfaces are returned; however, @flags can be used to filter 
-- the results for a smaller list of targeted interfaces. The valid flags are 
-- divided into groups, where each group contains bits that describe mutually exclusive 
-- attributes of a interface, and where all bits within a group describe all possible interfaces.
--
{-
connectListAllInterfaces :: Connection -> [ConnectListAllInterfacesFlags] -> IO [Interface]
connectListAllInterfaces conn flags = alloca (\p -> do
    let p' = castPtr (p::Ptr (Ptr Interface))
    n <- {# call virConnectListAllInterfaces #} (connectionToPtr conn)
                                                (p')
                                                (flags2int flags)
    exceptionOnMinusOne n
    peekArray (fromIntegral n) =<< peek p)

-}
-- | Provides the number of active interfaces on the physical host.
{# fun virConnectNumOfDefinedInterfaces as connectNumOfDefinedInterfaces
    { connectionToPtr `Connection' } -> `Int' #}

-- | Provides the number of defined (inactive) interfaces on the physical host.
{# fun virConnectNumOfInterfaces as connectNumOfInterfaces
    { connectionToPtr `Connection' } -> `Int' #}
    
-- | Activate an interface (i.e. call "ifup").
-- 
-- If there was an open network config transaction at the time this
-- interface was defined (that is, if @interfaceChangeBegin@ had 
-- been called), the interface will be brought back down 
-- (and then undefined) if @interfaceChangeRollback@ is called. p *
-- iface:  pointer to a defined interface
-- flags:  extra flags; not used yet, so callers should always pass 0
-- Returns:  0 in case of success, -1 in case of error
interfaceCreate ifs = void $ fmap exceptionOnMinusOne ({# call virInterfaceCreate #} (interfaceToPtr ifs) 0)
     
-- | Define an interface (or modify existing interface configuration).

-- Normally this change in the interface configuration is immediately 
-- permanent/persistent, but if @interfaceChangeBegin@ has been 
-- previously called (i.e. if an interface config transaction is open), 
-- the new interface definition will only become permanent if 
-- @interfaceChangeCommit@ is called prior to the next reboot of the 
-- system running libvirtd. Prior to that time, it can be explicitly 
-- removed using @virInterfaceChangeRollback@, or will be automatically 
-- removed during the next reboot of the system running libvirtd.
interfaceDefineXML con xml = withCString xml $! \str ->
    fmap ptrToInterface ({# call virInterfaceDefineXML #} (connectionToPtr con) str 0)

-- | deactivate an interface (ie call "ifdown") 
--
-- This does not remove the interface from the config, and does not 
-- free the associated virInterfacePtr object.
--
-- If there is an open network config transaction at the time this interface 
-- is destroyed (that is, if 'virInterfaceChangeBegin' had been called), and 
-- if the interface is later undefined and then 'virInterfaceChangeRollback'
-- is called, the restoral of the interface definition will also bring the 
-- interface back up.
interfaceDestroy ifs = 
    fmap exceptionOnMinusOne ({# call virInterfaceDestroy #} (interfaceToPtr ifs) 0)

-- | Free the interface object. The interface itself is unaltered. 
-- The data structure is freed and should not be used thereafter.
{# fun virInterfaceFree as interfaceFree
    { interfaceToPtr `Interface' } -> `Int' exceptionOnMinusOne* #}

-- | Get the MAC for an interface as string. For more information about MAC see RFC4122.
-- TODO check livetime
{# fun virInterfaceGetMACString as interfaceGetMACString
    { interfaceToPtr `Interface' } -> `ByteString' packCString* #}

-- | Get the public name for that interface
-- TODO check lifetime
{# fun virInterfaceGetName as interfaceGetName 
    { interfaceToPtr `Interface' } -> `String' #}

-- | Undefine an interface, ie remove it from the config. 
-- This does not free the associated virInterfacePtr object.
{# fun virInterfaceUndefine as interfaceUndefine
    { interfaceToPtr `Interface' } -> `Int' exceptionOnMinusOne* #}

-- | Determine if the interface is currently running 
{# fun virInterfaceIsActive as interfaceIsActive
    { interfaceToPtr `Interface' } -> `Int' exceptionOnMinusOne* #}

{# fun virInterfaceLookupByMACString as interfaceLookupByMACString
    { connectionToPtr `Connection'
    , `String' } -> `Interface' ptrToInterface* #}

{# fun virInterfaceGetXMLDesc as interfaceGetXMLDesc
    { interfaceToPtr `Interface'
    , `Int' } -> `String' #}

{# fun virInterfaceLookupByName as interfaceLookupByName
    { connectionToPtr `Connection'
    , `String' } -> `Interface' ptrToInterface* #}
