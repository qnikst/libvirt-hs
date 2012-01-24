{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import qualified Control.Exception as E

import System.LibVirt

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
  createDomain winxp `catchVirtError` (\e -> do
                                             putStrLn "Error:"
                                             print e)

  closeConnection c
  return ()
