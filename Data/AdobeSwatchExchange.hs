{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{- |

This module defines the types for 'AdobeStageExchange' and a suitable
'Binary' instance. All the get/put helper functions are also exported,
but, in general, you will just want the types and the 'Binary'
instance.

-}
module Data.AdobeSwatchExchange where

import Control.Applicative            ((<$>))
import Control.Monad                  (replicateM)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.Char                      (chr, ord)
import Data.Data                      (Data, Typeable)
import Data.Word                      (Word16)
import Data.Binary.IEEE754            (getFloat32be, putFloat32be)
import Numeric                        (showHex)
import Data.Binary                    (Binary(..))
import Data.Binary.Get                ( Get, getByteString, getWord8, getWord16be, getWord32be, runGet )
import Data.Binary.Put                ( Put, putByteString, putWord8, putWord16be, putWord32be, runPut )

{-
But to summarize, look for the following elements (in no particular order):
- header
- version
- "block count"
- blocks
- color group start
- color group end
- swatches
- "zero block"

- strings

For swatch "blocks"
- swatch header
- block length
- swatch name
- color space
- color component values (single-precision floats)
- swatch attributes

The only thing to really watch out for is that the count near the beginning is *not* the swatch count, but the number of "blocks" in the file, which also includes color group information.

File signature 4*char (ASEF)
Version 2*int8 (1.0)
Number of blocks 1*int16
Blocks
Block type (0xc001 ⇒ Group start, 0xc002 ⇒ Group end, 0x0001 ⇒ Color entry)
Block length 1*int16
Group/Color name 0-terminated string of length (int16) double-byte characters
Color model 4*char (CMYK, RGB, LAB or Gray)
Color values CMYK ⇒ 4*float16 / RGB & LAB ⇒ 3*float16 / Gray ⇒ 1*float16
Color type 1*int8 (0 ⇒ Global, 1 ⇒ Spot, 2 ⇒ Normal)

-}

-- | A color
data Color
    = CYMK Float Float Float Float
    | RGB Float Float Float
    | LAB Float Float Float
    | Gray Float
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | color type
data ColorType
    = Global
    | Spot
    | Normal
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | A named color
data ColorEntry = ColorEntry
    { colorName :: String
    , color     :: Color
    , colorType :: ColorType
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | An Adobe Swatch Exchange block
data ASEBlock
    = GroupStart { groupName :: String }
    | GroupEnd
    | CE ColorEntry
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | AdobeSwatchExchange
data AdobeSwatchExchange = AdobeSwatchExchange
    { version :: (Word16, Word16)
    , blocks  :: [ASEBlock]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable)

-- | get the ASEF file signature
getFileSig :: Get ()
getFileSig =
    do bs <- getByteString 4
       case bs of
         "ASEF" -> return ()
         _ -> fail $ "Invalid file signature: " ++ show bs

putFileSig :: Put
putFileSig = putByteString "ASEF"

getVersion :: Get (Word16, Word16)
getVersion =
    do maj <- getWord16be
       min <- getWord16be
       return (maj, min)

putVersion :: (Word16, Word16) -> Put
putVersion (maj, min) =
    do putWord16be maj
       putWord16be min

getBlock :: Get ASEBlock
getBlock =
    do blockType <- getWord16be
       case blockType of
         0xc001 -> do bl <- getWord32be
                      n <- getName
                      return (GroupStart n)
         0xc002 -> do bl <- getWord32be
                      return GroupEnd
         0x0001 -> CE <$> getColorEntry
         _      -> fail $ "Unknown block type: " ++ (showHex blockType "")

putBlock :: ASEBlock -> Put
putBlock (GroupStart groupName) =
    do putWord16be 0xc001
       putWord32be $ blName groupName
       putName groupName
putBlock GroupEnd =
    do putWord16be 0xc002
       putWord32be 0
putBlock (CE colorEntry) =
    do putWord16be 0x0001
       putColorEntry colorEntry

getName :: Get String
getName =
    do nameLength  <- getWord16be
       doubleChars <- replicateM ((fromIntegral nameLength) - 1) getWord16be
       _ <- getWord16be
       return $ map (chr . fromIntegral) doubleChars

putName :: String -> Put
putName nm =
    do putWord16be $ (fromIntegral (length nm) + 1)
       mapM_ (putWord16be . fromIntegral . ord) nm
       putWord16be 0

blName :: (Integral a) => String -> a
blName nm =
    fromIntegral (2 + (length nm * 2) + 2)

getColor :: Get Color
getColor =
    do modelString <- map (chr . fromIntegral) <$> replicateM 4 getWord8
       case modelString of
         "CYMK" -> do c <- getFloat32be
                      y <- getFloat32be
                      m <- getFloat32be
                      k <- getFloat32be
                      return $ CYMK c y m k
         "RGB " -> do r <- getFloat32be
                      g <- getFloat32be
                      b <- getFloat32be
                      return $ RGB r g b
         "LAB " -> do l <- getFloat32be
                      a <- getFloat32be
                      b <- getFloat32be
                      return $ LAB l a b
         "Gray" -> do g <- getFloat32be
                      return $ Gray g
         _      -> fail $ "Unknown color model: " ++ modelString

putColor :: Color -> Put
putColor (CYMK c y m k) =
    do mapM_ (putWord8 . fromIntegral . ord) "CYMK"
       putFloat32be c
       putFloat32be y
       putFloat32be m
       putFloat32be k
putColor (RGB r g b) =
    do mapM_ (putWord8 . fromIntegral . ord) "RGB "
       putFloat32be r
       putFloat32be g
       putFloat32be b
putColor (LAB l a b) =
    do mapM_ (putWord8 . fromIntegral . ord) "LAB "
       putFloat32be l
       putFloat32be a
       putFloat32be b
putColor (Gray g) =
    do mapM_ (putWord8 . fromIntegral . ord) "Gray"
       putFloat32be g

blColor :: (Integral a) => Color -> a
blColor (CYMK {}) = 20
blColor (RGB {})  = 16
blColor (LAB {})  = 16
blColor (Gray {}) = 8

getColorType :: Get ColorType
getColorType =
    do ct <- getWord16be
       case ct of
         0 -> return Global
         1 -> return Spot
         2 -> return Normal
         _ -> fail $ "Unknown color type: " ++ show ct

putColorType :: ColorType -> Put
putColorType Global = putWord16be 0
putColorType Spot   = putWord16be 1
putColorType Normal = putWord16be 2

instance Binary ColorType where
    put = putColorType
    get = getColorType

blColorType :: (Integral a) => a
blColorType = fromIntegral 2

getColorEntry :: Get ColorEntry
getColorEntry =
    do bl    <- getWord32be
       nm    <- getName
       color <- getColor
       typ   <- getColorType
       return $ ColorEntry { colorName = nm
                           , color     = color
                           , colorType = typ
                           }

putColorEntry :: ColorEntry -> Put
putColorEntry (ColorEntry cn c ct) =
    do putWord32be (blName cn + blColor c + blColorType)
       putName cn
       putColor c
       putColorType ct

instance Binary ColorEntry where
    put = putColorEntry
    get = getColorEntry

getASE :: Get AdobeSwatchExchange
getASE =
    do getFileSig
       v <- getVersion
       numBlocks <- getWord32be
       bs <- replicateM (fromIntegral numBlocks) getBlock -- unsafe-ish if there are billions of blocks
       return $ AdobeSwatchExchange { version = v
                                    , blocks  = bs
                                    }

putASE :: AdobeSwatchExchange -> Put
putASE (AdobeSwatchExchange v blks) =
    do putFileSig
       putVersion v
       putWord32be (fromIntegral $ length blks)
       mapM_ putBlock blks

instance Binary AdobeSwatchExchange where
    put = putASE
    get = getASE

-- | Convert a 'Color' to an RGB hex value.
colorToHex :: Color -> String
colorToHex (RGB r g b) =
    showString "#" .
    showHex' (round (r * 255)) .
    showHex' (round (g * 255)) .
    showHex' (round (b * 255)) $ ""
    where
      showHex' n
        | n < 10 = showString "0" . showHex n
        | otherwise  = showHex n
colorToHex c =
    error $ "Alas! We have not written the code to convert " ++ show c ++ " to the RGB color space."
