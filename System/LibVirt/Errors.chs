{-# LANGUAGE ForeignFunctionInterface #-}

{# context lib="virt" prefix="vir" #}

#include <libvirt/virterror.h>

module System.LibVirt.Errors where

import Foreign
import Foreign.C.Types
import Foreign.C.String

{# import System.LibVirt.Foreign #}

{# enum ErrorLevel {underscoreToCase} deriving (Eq, Show) #}
{# enum ErrorDomain {underscoreToCase} deriving (Eq, Show) #}

data Error = Error {
  veCode :: ErrorNumber,
  veDomain :: ErrorDomain,
  veMessage :: String,
  veLevel :: ErrorLevel,
  veConnect :: Connection,
  veDom :: Domain,
  veStr1 :: String,
  veStr2 :: String,
  veStr3 :: String,
  veInt1 :: Int,
  veInt2 :: Int,
  veNet :: Network }
  deriving (Eq, Show)

{# pointer *virErrorPtr as VirtErrorPtr -> Error #}

{# enum ErrorNumber {underscoreToCase} deriving (Eq, Show) #}

convertError :: Ptr () -> IO Error
convertError ptr = do
  let err = castPtr ptr :: VirtErrorPtr
  code <- {# get Error->code #} err
  domain <- {# get Error->domain #} err
  message <- peekCString =<< {# get Error->message #} err
  level <- {# get Error->level #} err
  conn <- {# get Error->conn #} err
  dom <- {# get Error->dom #} err
  str1 <- peekCString =<< {# get Error->str1 #} err
  str2 <- peekCString =<< {# get Error->str2 #} err
  str3 <- peekCString =<< {# get Error->str3 #} err
  int1 <- {# get Error->int1 #} err
  int2 <- {# get Error->int2 #} err
  net <- {# get Error->net #} err
  return $ Error {
             veCode = toEnum $ fromIntegral code,
             veDomain = toEnum $ fromIntegral domain,
             veMessage = message,
             veLevel = toEnum $ fromIntegral level,
             veConnect = ptrToConnection conn,
             veDom = ptrToDomain dom,
             veStr1 = str1,
             veStr2 = str2,
             veStr3 = str3,
             veInt1 = fromIntegral int1,
             veInt2 = fromIntegral int2,
             veNet = ptrToNetwork net }

{# fun virGetLastError as getLastError { } -> `Error' convertError* #}

