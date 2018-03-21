{-|
Module      : W
Description : Short description
Copyright   : (c) Some Guy, 2013
                  Someone Else, 2014
License     : GPL-3
Maintainer  : sample@email.com
Stability   : experimental
Portability : POSIX

Here is a longer description of this module, containing some
commentary with @some markup@.
-}

import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

import Diagrams hiding (text) 
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Layout.Constrained

-- import Graphics.SVGFonts
import Diagrams.Backend.PGF
import Data.Default.Class

import Data.Colour.Names

import Data.Semigroup
import Data.Maybe

import Control.Newtype hiding (under)

{--
-- The style for the diagram. 
data DiagramStyle n where
  DiagramStyle :: (Read n, RealFloat n) => {
    serifText    :: n -> TextOpts n
  , monoText       :: n -> TextOpts n
  , subscriptScale :: n
  , superscriptScale :: n
  } -> DiagramStyle n 

instance (Read n, RealFloat n) => Default (DiagramStyle n) where
  def = DiagramStyle{..}
    where
      serifText = \ h -> def {textHeight = h}
      monoText  = \ h -> def {textHeight = h, textFont = bit}
      subscriptScale = 0.4
      superscriptScale = 0.4
  

newtype PText b n = PText { getDiag :: QDiagram b V2 n Any }

instance Newtype (PText b n) (QDiagram b V2 n Any) where
  pack = PText
  unpack = getDiag 

under :: (Newtype o n, Newtype o' n') => (n -> n') -> o -> o'
under f = pack . f . unpack 

instance (Num n, Ord n, Floating n) =>  Semigroup (PText b n) where
  (PText a) <> (PText b) = PText $ beside (r2 (1,0)) a b
  
instance (Num n, Ord n, Floating n) =>  Monoid (PText b n) where
  mempty = PText mempty
  mappend = (<>)

text :: (TypeableFloat n, Renderable (Path V2 n) b)
     => TextOpts n -> String -> PText b n
text opts = PText . textSVG_ opts

subscript :: (?sty :: DiagramStyle n, TypeableFloat n, Renderable (Path V2 n) b)
          => PText b n -> PText b n
subscript = under $ centerY . scale subscriptScale
  where
    DiagramStyle{..} = ?sty
          
--}

-- constrCircleSq :: Diagram B
-- constrCircleSq = frame 0.2 $ layout $ do
--   c <- newDia (circle 1)
--   s <- newDia (square 2)
--   constrainWith hcat [c, s]
-- 
-- myCircle :: Diagram B
-- myCircle = circle 1
-- 
-- diagonalLayout :: Diagram B
-- diagonalLayout = frame 1 $ layout $ do
--   cirs <- newDias (map circle [1..5] # fc blue)
--   sqs  <- newDias (replicate 5 (square 2) # fc orange)
--   constrainWith vcat cirs
--   zipWithM_ sameY cirs sqs
--   constrainWith hcat [cirs !! 0, sqs !! 0]
--   along (direction (1 ^& (-1))) (map centerOf sqs)
  
-- use ShakeArgOptionsWith to gather things like width and height information
-- for diagrams, that way we can explicitly limit the flags we allow.
-- In addtion we take each diagram we build and put it in a named list so that
-- we can render it with the 
main :: IO ()
main = mainWith @(Diagram B) . surfOnlineTex latexSurface $ hboxOnline "$$f_{cons}$$"

-- shakeArgs shakeOptions{shakeFiles="_build"} $ return ()
--    want ["_build/run" <.> exe]
--
--    phony "clean" $ do
--        putNormal "Cleaning files in _build"
--        removeFilesAfter "_build" ["//*"]
--
--    "_build/run" <.> exe %> \out -> do
--        cs <- getDirectoryFiles "" ["//*.c"]
--        let os = ["_build" </> c -<.> "o" | c <- cs]
--        need os
--        cmd_ "gcc -o" [out] os
--
--    "_build//*.o" %> \out -> do
--        let c = dropDirectory1 $ out -<.> "c"
--        let m = out -<.> "m"
--        cmd_ "gcc -c" [c] "-o" [out] "-MMD -MF" [m]
--        needMakefileDependencies m
