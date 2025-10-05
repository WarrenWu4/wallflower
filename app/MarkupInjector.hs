module MarkupInjector (
  createTempFile,
  createImageMarkup
) where

-- | writes temporary file with image ui markup
createTempFile :: String -> IO ()
createTempFile content = do
  let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<interface>\n\t<requires lib=\"gtk\" version=\"4.0\"/>\n"
  let footer = "\n</interface>"
  writeFile "resources/images.ui" $ header ++ content ++ footer

-- | for an image path, creates a GtkPicture widget
createImageMarkup:: FilePath -> Int -> String
createImageMarkup path imageNum = "\t<object class=\"GtkPicture\" id=\"background-image-" ++ show imageNum ++ "\">\n\t\t<property name=\"file\">" ++ path ++ "</property>\n\t</object>\n"
