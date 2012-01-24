{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables, StandaloneDeriving, DeriveDataTypeable #-}

{# context lib="virt" prefix="vir" #}

#include <libvirt/virterror.h>

module System.LibVirt.Errors
  (Error (..), ErrorLevel (..),
   ErrorDomain (..), ErrorNumber (..),
   catchVirtError,
   exceptionOnMinusOne,
   ptrToConnection, ptrToDomain, ptrToNetwork,
   connectionToPtr, domainToPtr, networkToPtr)
  where

import qualified Control.Exception as E
import Data.Generics
import Foreign
import Foreign.C.Types
import Foreign.C.String

{# import System.LibVirt.Internal #}

{# enum ErrorLevel {underscoreToCase} deriving (Eq, Show) #}
deriving instance Data ErrorLevel
deriving instance Typeable ErrorLevel

{# enum ErrorDomain {underscoreToCase} deriving (Eq, Show) #}
deriving instance Data ErrorDomain
deriving instance Typeable ErrorDomain

data Error = Error {
  veCode :: ErrorNumber,
  veDomain :: ErrorDomain,
  veMessage :: String,
  veLevel :: ErrorLevel,
  veConnect :: Connection,
  veDom :: Domain,
  veStr1 :: Maybe String,
  veStr2 :: Maybe String,
  veStr3 :: Maybe String,
  veInt1 :: Int,
  veInt2 :: Int,
  veNet :: Network }
  deriving (Eq, Show, Data, Typeable)

instance E.Exception Error

{# pointer *virErrorPtr as VirtErrorPtr -> Error #}

{# enum ErrorNumber {underscoreToCase} deriving (Eq, Show) #}

deriving instance Data ErrorNumber
deriving instance Typeable ErrorNumber

data UnknownError = UnknownError
  deriving (Eq, Show, Data, Typeable)

instance E.Exception UnknownError

peekCString' :: CString -> IO (Maybe String)
peekCString' ptr
  | ptr == nullPtr = return Nothing
  | otherwise = Just `fmap` peekCString ptr

convertError :: Ptr () -> IO (Maybe Error)
convertError ptr
  | ptr == nullPtr = return Nothing
  | otherwise = do
  let err = castPtr ptr :: VirtErrorPtr
  code <- {# get Error->code #} err
  domain <- {# get Error->domain #} err
  message <- peekCString =<< {# get Error->message #} err
  level <- {# get Error->level #} err
  conn <- ptrToConnection' =<< {# get Error->conn #} err
  dom  <- ptrToDomain' =<< {# get Error->dom #} err
  str1 <- peekCString' =<< {# get Error->str1 #} err
  str2 <- peekCString' =<< {# get Error->str2 #} err
  str3 <- peekCString' =<< {# get Error->str3 #} err
  int1 <- {# get Error->int1 #} err
  int2 <- {# get Error->int2 #} err
  net  <- ptrToNetwork' =<< {# get Error->net #} err
  return $ Just $ Error {
             veCode = toEnum $ fromIntegral code,
             veDomain = toEnum $ fromIntegral domain,
             veMessage = message,
             veLevel = toEnum $ fromIntegral level,
             veConnect = conn,
             veDom = dom,
             veStr1 = str1,
             veStr2 = str2,
             veStr3 = str3,
             veInt1 = fromIntegral int1,
             veInt2 = fromIntegral int2,
             veNet = net }

{# fun virGetLastError as getLastError { } -> `Maybe Error' convertError* #}

catchVirtError :: IO a -> (Error -> IO b) -> IO (Either b a)
catchVirtError m f = do
  (Right `fmap` m) `E.catch` (\e -> Left `fmap` f e)

exceptionOnMinusOne :: CInt -> IO Int
exceptionOnMinusOne x = do
  if x == -1
    then handleError' (fromIntegral x)
    else return (fromIntegral x)

handleError :: IO a
handleError = do
    merr <- getLastError
    case merr of
      Just err -> E.throw err
      Nothing  -> E.throw UnknownError

handleError' :: a -> IO a
handleError' x = do
    merr <- getLastError
    case merr of
      Just err -> E.throw err
      Nothing  -> return x

ptrToConnection :: Ptr () -> IO Connection
ptrToConnection ptr
  | ptr == nullPtr = handleError
  | otherwise      = return $ Connection (castPtr ptr)

ptrToConnection' :: Ptr () -> IO Connection
ptrToConnection' ptr = return $ Connection (castPtr ptr)

connectionToPtr :: Connection -> Ptr ()
connectionToPtr (Connection ptr) = castPtr ptr

ptrToDomain :: Ptr () -> IO Domain
ptrToDomain ptr
  | ptr == nullPtr = handleError
  | otherwise      = return $ Domain (castPtr ptr)

ptrToDomain' :: Ptr () -> IO Domain
ptrToDomain' ptr = return $ Domain (castPtr ptr)

domainToPtr :: Domain -> Ptr ()
domainToPtr (Domain ptr) = castPtr ptr

ptrToNetwork :: Ptr () -> IO Network
ptrToNetwork ptr
  | ptr == nullPtr = handleError
  | otherwise      = return $ Network (castPtr ptr)

ptrToNetwork' :: Ptr () -> IO Network
ptrToNetwork' ptr = return $ Network (castPtr ptr)

networkToPtr :: Network -> Ptr ()
networkToPtr (Network ptr) = castPtr ptr

