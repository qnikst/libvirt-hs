
module System.LibVirt
  (module System.LibVirt.Foreign,
   module System.LibVirt.Errors,
   withConnection
  ) where

import System.LibVirt.Foreign
import System.LibVirt.Errors

withConnection :: String -> (Connection -> IO a) -> IO a
withConnection uri fn = do
  conn <- openConnection uri
  result <- fn conn
  closeConnection conn
  return result

