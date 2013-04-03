module Data.AdobeSwatchExchange.CSS where

import Data.AdobeSwatchExchange       as ASE (ASEBlock(CE), ColorEntry(color), AdobeSwatchExchange, Color(RGB), blocks, getASE)
import qualified Data.ByteString.Lazy as B
import Language.Css.Build             ((<:>), (/.), cword, ruleSets, star)
import Language.Css.Build.Idents      as CSS (backgroundColor, color)
import Language.Css.Syntax            (Expr, RuleSet, StyleSheet)
import Numeric                        (showHex)

ase2css :: [ColorEntry] -> StyleSheet
ase2css colorEntries =
    ruleSets (concatMap genColor (zip [1..] colorEntries))

genColor :: (Int, ColorEntry) -> [RuleSet]
genColor (n, colorEntry) =
    [ (star /. ("fg-color-ase-" ++ Prelude.show n)) [ CSS.color <:> (colorToHex (ASE.color colorEntry))
                                                    ]
    , (star /. ("bg-color-ase-" ++ Prelude.show n)) [ backgroundColor <:> (colorToHex (ASE.color colorEntry))
                                                    ]
    ]

colorToHex :: Color -> Expr
colorToHex (RGB r g b) =
    cword $
    showString "#" .
    showHex' (round (r * 255)) .
    showHex' (round (g * 255)) .
    showHex' (round (b * 255)) $ ""
    where
      showHex' n
        | n < 10 = showString "0" . showHex n
        | otherwise  = showHex n
