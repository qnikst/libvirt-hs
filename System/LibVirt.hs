
module System.LibVirt
  (module System.LibVirt.Foreign,
   module System.LibVirt.Errors,
   withConnection
  ) where

import System.LibVirt.Foreign
import System.LibVirt.Errors
import Control.Exception(finally)

withConnection :: String -> (Connection -> IO a) -> IO a
withConnection uri fn = do
  conn <- openConnection uri
  finally (fn conn) (closeConnection conn)

