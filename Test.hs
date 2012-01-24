
import LibVirt

main = do
  initialize
  c <- openConnection "qemu:///system"
  n <- domainsCount c
  print n
  ids <- domainsIDs c
  print ids
  closeConnection c
  return ()
