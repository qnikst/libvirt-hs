{-# LANGUAGE ScopedTypeVariables #-}

import Control.Monad
import Control.Concurrent
import System.Environment
import Foreign

import System.LibVirt

uri = "qemu:///system"

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["test"]             -> initialize >> withConnection uri doTest
    ["list-nets"]        -> initialize >> withConnection uri listNets
    ["start", domain]    -> initialize >> withConnection uri (start domain)
    ["stop",  domain]    -> initialize >> withConnection uri (stop  domain)
    ["destroy", domain]  -> initialize >> withConnection uri (destroy domain)
    ["callback", domain] -> initialize >> eventRegisterDefaultImpl >> withConnection uri (doTest1 domain)
    ["capabilities"]     -> initialize >> withConnection uri (\c -> connectGetCapabilities c >>= putStrLn)
    other -> putStrLn "Usage: Test <test | list-nets | start DOMAIN | stop DOMAIN | destroy DOMAIN | callback DOMAIN | capabilities>"

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

destroy:: String -> Connection -> IO ()
destroy domain conn = do
  putStrLn $ "Destroing down  " ++ domain ++ "..."
  dom <- lookupDomainName conn domain
  destroyDomain dom `catchVirtError` (\e -> do
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

listNets :: Connection -> IO ()
listNets conn = do
  putStrLn $ "Defined networks:"
  names <- definedNetworksNames conn
  forM_ names putStrLn
  putStrLn ""
  nr <- runningNetworksCount conn
  putStrLn $ "Number of running networks: " ++ show nr
  putStrLn "Running networks:"
  names <- runningNetworksNames conn
  forM_ names putStrLn

doTest1 domain conn = do

  doTest conn
  dom <- lookupDomainName conn domain
  print $ fromEnum DomainEventIdLifecycle
  callback <- mkConnectDomainEventCallback $ \conn dom event detail _o -> do
      print conn
      print =<< getDomainInfo dom
      let ev = toEnum event
      print ev
      case ev of
         DomainEventDefined -> print (toEnum detail ::DomainEventDefinedDetailType)
         DomainEventUndefined -> print (toEnum detail ::DomainEventUndefinedDetailType)
         DomainEventStarted -> print (toEnum detail :: DomainEventStartedDetailType)
         DomainEventSuspended -> print (toEnum detail :: DomainEventSuspendedDetailType)
         DomainEventResumed -> print (toEnum detail :: DomainEventResumedDetailType)
         DomainEventStopped -> print (toEnum detail :: DomainEventStoppedDetailType)
         DomainEventShutdown -> print (toEnum detail :: DomainEventShutdownDetailType)
      print "callback"
  callbackFree <- mkFreeCallback $ \a -> print "free"
  --start domain conn
  --threadDelay (10*1000000)
  print callback
  print callbackFree
  print conn
  print dom
  print =<< getDomainInfo dom
  connectDomainEventRegisterAny conn (dom) 
                                     DomainEventIdLifecycle
                                     (callback) -- callback
                                     (nullPtr :: Ptr Int) 
                                     (callbackFree) -- callbackFree
  forkIO . forever $ eventRunDefaultImpl
  getLine
  freeHaskellFunPtr callback
  freeHaskellFunPtr callbackFree

