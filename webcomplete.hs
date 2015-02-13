{-# LANGUAGE OverloadedStrings, TemplateHaskell, UnicodeSyntax #-}

module Main
( main
) where

import           Control.Applicative         ((<$>))
import qualified Data.ByteString.Char8       as BS
import           Network.Wai.Middleware.Cors
import           Paths_webcomplete
import           Text.FastEdit               (buildDict)
import           Web.Scotty

main :: IO ()
main = do
  neighbours <-  buildDict 2 . map BS.unpack  . BS.lines
                 <$> (BS.readFile =<< getDataFileName "/usr/share/dict/words")
  scotty 8080 $ do
    middleware simpleCors
    get  "/complete" $ do
      json . neighbours =<< param "partial"
