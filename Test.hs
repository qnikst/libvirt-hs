{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import System.Environment

import System.LibVirt

uri = "qemu:///system"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"]          -> initialize >> withConnection uri doTest
    ["start", domain] -> initialize >> withConnection uri (start domain)
    ["stop",  domain] -> initialize >> withConnection uri (stop  domain)
    other -> putStrLn "Usage: Test <test | start DOMAIN | stop DOMAIN>"

start :: String -> Connection -> IO ()
start domain conn = do
  putStrLn $ "Running " ++ domain ++ "..."
  dom <- lookupDomainName conn domain
  createDomain dom `catchVirtError` (\e -> do
                                           putStrLn "Error:"
                                           print e)
  return ()

stop :: String -> Connection -> IO ()
stop domain conn = do
  putStrLn $ "Shutting down  " ++ domain ++ "..."
  dom <- lookupDomainName conn domain
  shutdownDomain dom `catchVirtError` (\e -> do
                                             putStrLn "Error:"
                                             print e)
  return ()

doTest :: Connection -> IO ()
doTest conn = do
  putStrLn $ "Defined domains:"
  names <- definedDomainsNames conn
  forM_ names $ \name -> do
    putStrLn $ "  Domain: " ++ name
    di <- getDomainInfo =<< lookupDomainName conn name
    putStrLn $ "  Info: " ++ show di

  nr <- runningDomainsCount conn
  putStrLn $ "Number of running domains: " ++ show nr

