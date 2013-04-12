{- |

Create a @.less@ file from a @.ase@ file. For each color in the @.ase@ the @.css@ will contain an entry:

    color-ase-n

Where @n@ is an integer based on the order the color was found in the @.ase@ file.

-}
module Data.AdobeSwatchExchange.LESS where

import Control.Monad.State            (State, evalState, get, put)
import Data.AdobeSwatchExchange       as ASE (ASEBlock(CE, GroupStart, GroupEnd), ColorEntry(color), AdobeSwatchExchange, Color(RGB), blocks, colorToHex, getASE)
import qualified Data.ByteString.Lazy as B
import Numeric                        (showHex)
import Text.PrettyPrint.HughesPJ      (Doc, (<>), (<+>), ($+$), braces, colon, empty, int, semi, text, vcat)

-- | generate a less document from an 'AdobeSwatchExchange'
ase2less :: AdobeSwatchExchange -> Doc
ase2less ase =
    vcat $ evalState (mapM genColor (blocks ase)) 1

-- | generate a color rules from an indexed @ColorEntry@
--
-- generates:
--
--    fg-color-ase-n
--    bg-color-ase-n
--    border-color-ase-n
--
-- Currently only 'RGB' color is supported. Since CSS only supports
-- rgb, supporting other colors would require the conversion to RGB
-- color space. Feel free to send a patch!
genColor :: ASEBlock -> State Int Doc
genColor (CE colorEntry) =
    do n <- get
       put $! (n + 1)
       let colorAse = text "@color-ase-" <> int n
       return $ colorAse <> colon <+> (text $ colorToHex (color colorEntry)) <> semi $+$
                text ".fg-color-ase-" <> int n <+> braces (text "color:" <+> colorAse <> semi) $+$
                text ".bg-color-ase-" <> int n <+> braces (text "background-color:" <+> colorAse <> semi) $+$
                text ".border-color-ase-" <> int n <+> braces (text "border-color:" <+>  colorAse <> semi) $+$
                text ""
genColor (GroupStart name) =
    return $ text "/* Group" <+> text name  <+> text "*/" -- FIXME: what if name contains */
genColor _ = return empty
