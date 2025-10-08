{-# LANGUAGE ForeignFunctionInterface #-}

module FontBindings (addFontFile) where

import Data.Text (Text, unpack)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CBool (..))
import System.IO.Unsafe (unsafePerformIO)

-- | The foreign import declaration.
--
-- 'ccall' specifies the C calling convention.
-- 'unsafe' is used because the C function doesn't call back into Haskell or
-- involve GHC runtime state (it's a simple, short operation).
-- "hs_add_font_file" is the exact name of the C function defined above.
foreign import ccall unsafe "hs_add_font_file"
  c_add_font_file :: CString -> IO CBool

-- | Haskell wrapper function to handle the Text-to-CString conversion and IO.
-- Returns True on success, False on failure.
addFontFile :: Text -> IO Bool
addFontFile path = do
  -- Convert the Haskell Text path to a CString (null-terminated byte array)
  withCString (unpack path) $ \c_path -> do
    -- Call the C function
    result <- c_add_font_file c_path
    -- Convert the C boolean result back to a Haskell Bool
    return (result /= 0)
