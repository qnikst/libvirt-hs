
import Control.Monad

import LibVirt

main = do
  initialize
  c <- openConnection "qemu:///system"

  putStrLn $ "All domains:"
  names <- definedDomainsNames c
  forM_ names $ \name -> do
    putStrLn $ "  Domain: " ++ name
    di <- getDomainInfo =<< lookupDomainName c name
    putStrLn $ "  Info: " ++ show di

  nr <- runningDomainsCount c
  putStrLn $ "Number of running domains: " ++ show nr

  putStrLn $ "Running WinXP..."

  winxp <- lookupDomainName c "WinXP"
  createDomain winxp

  closeConnection c
  return ()
