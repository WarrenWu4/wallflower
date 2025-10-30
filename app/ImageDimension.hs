module ImageDimension (getImageFormat, getImageDimension) where

-- helper module to get image dimension
-- dead fucking simple with little dependencies
-- supports: png

-- TODO: add jpg/jpeg support

import Data.Binary.Get (getWord32be, runGet)
import qualified Data.ByteString as B
import Data.Char (toLower)
import Data.List.Split (splitOn)

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

getJPEGDimensions :: FilePath -> IO (Maybe (Int, Int))
getJPEGDimensions path = do
  return $ Just (16, 9)
