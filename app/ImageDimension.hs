module ImageDimension (getImageFormat, getImageDimension) where

-- helper module to get image dimension
-- dead fucking simple with little dependencies
-- supports: png, jpeg

import Data.Binary.Get (getWord32be, runGet, skip, Get, getWord16be)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Word (Word8)
import Data.Bits (Bits(shiftR), (.&.))

getImageFormat :: String -> String
getImageFormat f = do
  map toLower $ last $ splitOn "." f

getImageDimension :: String -> String -> IO (Maybe (Int, Int))
getImageDimension path format
  | format == "png" = getPNGDimensions path
  | format == "jpg" || format == "jpeg" = getJPEGDimensions path
  | otherwise = return Nothing

getPNGDimensions :: String -> IO (Maybe (Int, Int))
getPNGDimensions path = do
  bs <- B.readFile path
  if B.length bs < 24
    then return Nothing
    else do
      let headerChunk = B.drop 16 bs
      let lazyHeaderChunk = B.fromStrict headerChunk
      let dimensions = runGet ((,) <$> getWord32be <*> getWord32be) lazyHeaderChunk
      let width = fromIntegral (fst dimensions)
      let height = fromIntegral (snd dimensions)
      return $ Just (width, height)

sofMarkers :: [Word8]
sofMarkers = [0xC0, 0xC1, 0xC2, 0xC3, 0xC5, 0xC6, 0xC7, 0xC9, 0xCA, 0xCB, 0xCD, 0xCE, 0xCF]

getJPEGDimensionsRecursive :: Get (Int, Int)
getJPEGDimensionsRecursive = do
  buffer <- getWord16be
  let shiftRightValue = buffer `shiftR` 8
  let firstByte = fromIntegral shiftRightValue :: Word8
  let maskedVal = buffer .&. 0x00FF
  let secondByte = fromIntegral maskedVal :: Word8
  -- check that the first byte is 0xFF indicating a marker
  if firstByte == 0xFF
    then do 
      if secondByte `elem` sofMarkers
        then do
          -- skip 3 bytes (length, precision)
          skip 3
          width <- getWord16be
          height <- getWord16be
          return (fromIntegral width, fromIntegral height)
        else do
          getJPEGDimensionsRecursive
    else do
      -- must be the length of marker which contains the size of payload
      -- skip the payload size to move on to the next marker
      skip $ fromIntegral buffer - 2
      getJPEGDimensionsRecursive

getJPEGDimensions :: FilePath -> IO (Maybe (Int, Int))
getJPEGDimensions path = do
  fileContent <- L.readFile path 
  case runGet getJPEGDimensionsRecursive fileContent of
    (width, height) ->
      return $ Just (width, height)
