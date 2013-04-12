module Main where

import Data.AdobeSwatchExchange.LESS (ase2less)
import Data.AdobeSwatchExchange (ASEBlock(CE), ColorEntry(colorName), AdobeSwatchExchange, blocks, getASE)
import qualified Data.ByteString.Lazy as B
import Data.Binary.Get                (runGet)
import System.Environment             (getArgs)
import Text.PrettyPrint.HughesPJ      (($+$), (<+>), doubleQuotes, text)

main :: IO ()
main =
    do args <- getArgs
       case args of
         [inFile, outFile] ->
             do c <- B.readFile inFile
                let ase  = runGet getASE c
                    less = text "/* Generated from" <+> doubleQuotes (text inFile) <+> text "using ase2less */" $+$
                           ase2less ase
                writeFile outFile $ show $ less
         _ -> putStrLn "Usage: ase2css <infile.ase> <outfile.less>"
