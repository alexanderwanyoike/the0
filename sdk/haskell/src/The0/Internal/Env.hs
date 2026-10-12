-- |
-- Module      : The0.Internal.Env
-- Description : Environment variables as the bytes the process was given
-- License     : Apache-2.0
module The0.Internal.Env
    ( lookupEnvBytes
    ) where

import qualified Data.ByteString as BS
import qualified GHC.Foreign as Foreign
import GHC.IO.Encoding (getFileSystemEncoding)
import System.Environment (lookupEnv)

-- | The value of an environment variable as the bytes the process was given.
--
-- 'lookupEnv' decodes those bytes with the locale's encoding, so JSON read
-- through 'String' and packed byte by byte loses any non-ASCII text under a
-- UTF-8 locale. Encoding the text back with the same encoding restores the
-- bytes exactly, whatever the locale.
lookupEnvBytes :: String -> IO (Maybe BS.ByteString)
lookupEnvBytes name = do
    value <- lookupEnv name
    encoding <- getFileSystemEncoding
    traverse (\text -> Foreign.withCStringLen encoding text BS.packCStringLen) value
