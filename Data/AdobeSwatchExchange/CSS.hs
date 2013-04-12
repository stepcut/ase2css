{- |

Create a @.css@ file from a @.ase@ file. For each color in the @.ase@ the @.css@ will contain three entries:

    fg-color-ase-n
    bg-color-ase-n
    border-color-ase-n

Where @n@ is an integer based on the order the color was found in the @.ase@ file.

-}
module Data.AdobeSwatchExchange.CSS where

import Control.Monad.State            (State, get, put, evalState)
import Data.AdobeSwatchExchange       as ASE (ASEBlock(CE), ColorEntry(color), AdobeSwatchExchange, Color(RGB), blocks, colorToHex, getASE)
import qualified Data.ByteString.Lazy as B
import Language.Css.Build             ((<:>), (/.), cword, ruleSets, star)
import Language.Css.Build.Idents      as CSS (backgroundColor, borderColor, color)
import Language.Css.Syntax            (Expr, RuleSet, StyleSheet)
import Numeric                        (showHex)

-- | generate a 'StyleSheet' from an 'AdobeSwatchExchange'
ase2css :: AdobeSwatchExchange -> StyleSheet
ase2css ase =
    ruleSets (concat $ evalState (mapM genBlock (blocks ase)) 1)

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
-- genColor :: (Int, ASEBlock) -> (Int, [RuleSet])
genBlock :: ASEBlock -> State Int [RuleSet]
genBlock (CE colorEntry) =
    do n <- get
       put $! (n + 1)
       return [ (star /. ("fg-color-ase-" ++ Prelude.show n))
                [ CSS.color <:> (cword $ colorToHex (ASE.color colorEntry))
                ]
              , (star /. ("bg-color-ase-" ++ Prelude.show n))
                [ backgroundColor <:> (cword $ colorToHex (ASE.color colorEntry))
                ]
              , (star /. ("border-color-ase-" ++ Prelude.show n))
                [ borderColor <:> (cword $ colorToHex (ASE.color colorEntry))
                ]
              ]
-- in theory, we should generate CSS comments for the GroupName, but
-- language-css does not support comments :-/
genBlock _ = return []
