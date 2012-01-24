
import Control.Monad

import LibVirt

main = do
  initialize
  c <- openConnection "qemu:///system"
  n <- domainsCount c
  print n
  ids <- domainsIDs c
  print ids
  forM_ ids $ \i -> do
    di <- getDomainInfo =<< lookupDomainID c i
    print di
  closeConnection c
  return ()
