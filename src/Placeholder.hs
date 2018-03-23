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

module Placeholder where

import GHC.Generics

import System.Random

import Data.Aeson
import qualified Data.Aeson as A 

import Data.Binary
import qualified Data.Binary as B

import Data.Tuple (swap) 
import Data.Maybe (catMaybes) 

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Data.Text.Lazy.Encoding as T 

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

import Data.Set (Set)
import qualified Data.Set as Set

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Map (Map)
import qualified Data.Map as Map

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data RawData a = RawData {
    script :: Text
  , id     :: Text
  , input  :: [[a]]
  , output :: a
  } deriving (Show, Read, Generic, Functor, Foldable, Traversable)

instance (ToJSON   a) => ToJSON   (RawData a)
instance (FromJSON a) => FromJSON (RawData a) 
instance (Binary   a) => Binary   (RawData a) 

{-
import Control.Lens ((^.), (^..), (.~), to)
import Control.Monad ((>=>), zipWithM, when, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Loops (unfoldrM)
import Data.Aeson.Lens (values, key, _String)
import Data.Char (toLower, isAscii)
import Data.Int (Int32, Int64)
import Data.List (genericLength)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.ProtoLens (decodeMessageOrDie)
import Data.Random (StdRandom(..))
import Data.Random.Extras (sample)
import Data.Random.RVar (runRVar)
import Data.Tuple (swap)
import Debug.Trace
import MonadUtils (mapAccumLM)
import Options.Applicative as OptParse
import Proto.Tensorflow.Core.Framework.Summary (Summary)
import System.FilePath.Glob (glob)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Vector.Storable as S

import qualified TensorFlow.Core as TF
import qualified TensorFlow.GenOps.Core as TF (sigmoid, tanh, div, rank, squeeze', multinomial, applyAdam, l2Loss)
import qualified TensorFlow.Gradient as TF
import qualified TensorFlow.Initializers as TF
import qualified TensorFlow.Logging as TF
import qualified TensorFlow.Minimize as TF
import qualified TensorFlow.Ops as TF hiding (initializedVariable, zeroInitializedVariable)
import qualified TensorFlow.RNN as TF
import qualified TensorFlow.Variable as TF
-}


-- | Group a list of elements in the sublists of length @i@
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk i xs = let (f, r) = splitAt i xs in f : chunk i r

-- * Read the raw input data into a lazy list of records.
readRawData :: FilePath -> IO [RawData Text]
readRawData = fmap (catMaybes . map (A.decode . T.encodeUtf8) . T.lines) . T.readFile  

type Count     = Int
type ID        = Int
type SymbolSet = Map Text Count
type SymbolMap = Map Text ID
type IDMap     = IntMap Text
type IDSet     = IntSet 

-- * get all the symbols from a single raw record
gatherSymbols :: (Ord a) => RawData a -> Set a
gatherSymbols = foldMap Set.singleton

-- * gather all the symbols from a list of raw records
gatherAllSymbols :: [RawData Text] -> SymbolSet
gatherAllSymbols = Map.unionsWith (+)
                 . map (Map.fromSet (const 1) . gatherSymbols)

-- * Build the build symbol to integer and integer to symbol maps from a set of
--   symbols.
buildIDMaps :: (Ord a) => Set a -> (Map a Int, IntMap a)
buildIDMaps s = (sm, im)
  where
    pairs = zip (Set.toList s) [1..]
    sm = Map.fromList pairs
    im = IntMap.fromList . map swap $ pairs

-- * The generator we use for batching a set of elements. 
batchGen :: StdGen
batchGen = mkStdGen 3078038070 -- Just a random seed

-- * Take some list of inputs and 
makeBatches :: Int -> [a] -> IntMap [a]
makeBatches num list = foldr add IntMap.empty paired
  where
    randList :: [Int] 
    randList = randomRs (0, num - 1) batchGen
    --paired :: [(Int, a)]
    paired = zip randList list
    -- add :: (Int, a) -> IntMap [a] -> IntMap [a]
    add (k,v) = IntMap.insertWith (++) k [v] 

-- Work your way up with models
   -- V/ Simple
      -- Build 2 layer network to compressed rep
      -- sum compressed reps together
      -- have another 2 layer NN generate output then softmax
   -- W/ Embed
      -- Add embedding and de-embedding matrices at either end
   -- W/ 1dir-lstm
      -- Add an lstm
   -- W/ UNK filtering.
      -- Replace uncommon inputs with UNK?
   -- W/ input and output GRU/LSTM embeddings 

data Model = T
