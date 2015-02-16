{-# LANGUAGE OverloadedStrings, TemplateHaskell, UnicodeSyntax #-}

module Main
( main
) where

import           Control.Applicative         ((<$>))
import           Control.Monad.IO.Class      (liftIO)
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy        as BL
import           Data.FileEmbed              (embedFile)
import           Data.Text.Lazy.Encoding     (decodeUtf8)
import           Network.Wai.Middleware.Cors
import           Text.FastEdit               (buildDict)
import           Web.Scotty


dict = $(embedFile "/usr/share/dict/british-english")

neighbours = buildDict 3 . map BS.unpack  $ BS.lines dict

index =  $(embedFile "index.html")

main :: IO ()
main = do
  let indexText = decodeUtf8 (BL.fromChunks [index])
  
  print (neighbours "test")
  scotty 8080 $ do
    middleware simpleCors
    get  "/complete" $ do
      -- log queries to stdout
      params >>= liftIO . print
      json . neighbours =<< param "term"

    get "/" $ html indexText
