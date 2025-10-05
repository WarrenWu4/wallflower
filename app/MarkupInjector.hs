module MarkupInjector (
  createTempFile,
  createImageMarkup
) where

createTempFile :: String -> IO ()
createTempFile content = do
  let header = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<interface>\n\t<requires lib=\"gtk\" version=\"4.0\"/>\n"
  let footer = "\n</interface>"
  writeFile "resources/images.ui" header
  writeFile "resources/images.ui" content
  writeFile "resources/images.ui" footer

-- | for an image path, creates a GtkPicture widget
createImageMarkup:: FilePath -> String
createImageMarkup path = "<object class=\"GtkPicture\">\n<property name=\"filename\">" ++ path ++ "</property>\n</object>"
