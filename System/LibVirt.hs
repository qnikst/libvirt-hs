
module System.LibVirt
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
   F.openConnection, closeConnection,

   -- * Domains management functions
   runningDomainsCount, definedDomainsCount,
   F.runningDomainsIDs, F.definedDomainsNames,
   F.lookupDomainID, F.lookupDomainName,
   F.getDomainInfo, F.getDomainXML,
   F.defineDomainXML, undefineDomain,

   -- * Domains control
   createDomain, F.createDomainXML,
   destroyDomain, 
   shutdownDomain, rebootDomain,
   suspendDomain, resumeDomain,
   saveDomain, restoreDomain,
   refDomain, freeDomain,

   -- * Networks management
   F.getNetworkConnection,

   -- * Errors handling
   catchVirtError
  ) where

import System.LibVirt.Internal (Connection, Domain, Network)
import qualified System.LibVirt.Foreign as F
import System.LibVirt.Foreign (
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
   ConnectCredential (..) )
import System.LibVirt.Errors

initialize :: IO Int
initialize = exceptionOnMinusOne F.initialize

closeConnection :: Connection -> IO Int
closeConnection = exceptionOnMinusOne . F.closeConnection

runningDomainsCount :: Connection -> IO Int
runningDomainsCount = exceptionOnMinusOne . F.runningDomainsCount

definedDomainsCount :: Connection -> IO Int
definedDomainsCount = exceptionOnMinusOne . F.definedDomainsCount

createDomain :: Domain -> IO Int
createDomain = exceptionOnMinusOne . F.createDomain

undefineDomain :: Domain -> IO Int
undefineDomain = exceptionOnMinusOne . F.undefineDomain

destroyDomain :: Domain -> IO Int
destroyDomain = exceptionOnMinusOne . F.destroyDomain

shutdownDomain :: Domain -> IO Int
shutdownDomain = exceptionOnMinusOne . F.shutdownDomain

rebootDomain :: Domain -> IO Int
rebootDomain d = exceptionOnMinusOne $ F.rebootDomain d 0

suspendDomain :: Domain -> IO Int
suspendDomain = exceptionOnMinusOne . F.suspendDomain

resumeDomain :: Domain -> IO Int
resumeDomain = exceptionOnMinusOne . F.resumeDomain

saveDomain :: Domain -> FilePath -> IO Int
saveDomain d path = exceptionOnMinusOne $ F.saveDomain d path

restoreDomain :: Connection -> FilePath -> IO Int
restoreDomain d path = exceptionOnMinusOne $ F.restoreDomain d path

refDomain :: Domain -> IO Int
refDomain = exceptionOnMinusOne . F.refDomain

freeDomain :: Domain -> IO Int
freeDomain = exceptionOnMinusOne . F.freeDomain

