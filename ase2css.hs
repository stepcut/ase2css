module Main where

import Data.AdobeSwatchExchange.CSS (ase2css)
import Data.AdobeSwatchExchange (ASEBlock(CE), ColorEntry(colorName), AdobeSwatchExchange, blocks, getASE)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get                (runGet)
import Language.Css.Pretty            (pretty)
import System.Environment             (getArgs)
import Text.PrettyPrint.HughesPJ      (($+$), (<+>), doubleQuotes, text)

main :: IO ()
main =
    do args <- getArgs
       case args of
         [inFile, outFile] ->
             do c <- B.readFile inFile
                let ase = runGet getASE c
                    css = text "/* Generated from" <+> doubleQuotes (text inFile) <+> text "using ase2css */" $+$
                          pretty (ase2css [ ce | (CE ce) <- blocks ase ])
                writeFile outFile $ show $ css
         _ -> putStrLn "Usage: ase2css <infile.ase> <outfile.css>"

