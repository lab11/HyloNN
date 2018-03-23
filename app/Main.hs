
module Main where

import Placeholder

import Control.Monad

import Development.Shake
import Development.Shake.FilePath

import Text.Show.Pretty

import qualified Codec.Compression.GZip as GZ

import Data.Maybe (catMaybes) 

import Control.Applicative ((<|>))

import Data.Aeson
import qualified Data.Aeson as A 

import Data.Binary
import qualified Data.Binary as B

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

import Data.IntMap (IntMap,(!))
import qualified Data.IntMap as IntMap

-- * The directory in which we build all our files. Clean should just be
--   deleting this one director. 
workDir :: FilePath
workDir = "_build"

-- * Set of defaults for our build system. 
hylonnShakeOpts :: ShakeOptions
hylonnShakeOpts = shakeOptions{
    shakeFiles     = workDir
  , shakeVerbosity = Loud 
  }
  --, shakeProgress = progressSimple
  --, shakeThreads  = 0}

main :: IO ()
main = do

  putStrLn "Starting hylonn build process."
  
  shakeArgs hylonnShakeOpts $ do

    -- #### Caches for Repeatedly Used Files ####

    getRawBatchSet <- newCache $ \ k -> do
      need [k]
      putNormal $ "Reading rawBatchSet '" ++ k ++ "'."
      liftIO $ B.decode @[FilePath] . GZ.decompress <$> BS.readFile k

    getRawBatchChunk <- newCache $ \ k -> do
      need [k]
      putNormal $ "Reading rawBatchChunk '" ++ k ++ "'."
      liftIO $ B.decode @(IntMap [RawData Text]) . GZ.decompress <$> BS.readFile k

    getRawBatch <- newCache $ \ k -> do
      need [k]
      putNormal $ "Reading rawBatch '" ++ k ++ "'."
      liftIO $ B.decode @[RawData Text] . GZ.decompress <$> BS.readFile k

    getRawData <- newCache $ \ k -> do 
      need [k]
      putNormal $ "Reading rawData '" ++ k ++ "'."
      liftIO $ readRawData k

    getBatchSymbs <- newCache $ \ k -> do
      need [k]
      putNormal $ "Reading Batch Symbs '" ++ k ++ "'."
      liftIO $ B.decode @SymbolSet . GZ.decompress <$> BS.readFile k

    getBatchIDs <- newCache $ \ k -> do
      need [k]
      putNormal $ "Reading Batch IDs '" ++ k ++ "'."
      liftIO $ B.decode @IDSet . GZ.decompress <$> BS.readFile k
      
    getBatchSymbsMap <- newCache $ \ k -> do
      need [k]
      putNormal $ "Reading Batch Symbs Map '" ++ k ++ "'."
      liftIO $ B.decode @SymbolMap . GZ.decompress <$> BS.readFile k

    getBatchIDMap <- newCache $ \ k -> do
      need [k]
      putNormal $ "Reading Batch ID Map '" ++ k ++ "'."
      liftIO $ B.decode @IDMap . GZ.decompress <$> BS.readFile k
      
    getConvertedBatch <- newCache $ \ k -> do
      need [k]
      putNormal $ "Reading Converted Batch '" ++ k ++ "'."
      liftIO $ B.decode @[RawData Int] . GZ.decompress <$> BS.readFile k


    -- #### Directories for work ####
      
        -- archive we want to unpack 
    let dataArchive = workDir </> "mljs.v1" <.> "tgz"
        -- the folder containing unpacked files
        unpackDir   = workDir </> "mljs"
        -- the folder that contains various sorts of batches.
        batchDir    = workDir </> "batches" 

    -- #### Download and Unpack archive ####
      
    -- Grab the dataset from the internet. 
    dataArchive %> \ out -> do
      let url = "https://people.eecs.berkeley.edu/~ksen/mljs.v1.tgz"
          shasum = "ff688f905f07695314c460f7a66370162411e968  _build/mljs.v1.tgz"
          download = do
            putNormal "Downloading homework archive" 
            command [] "curl" [url, "-o", out]
      exists <- doesFileExist out
      -- This is a large fucking download, so if the file exists we should
      -- probably just check if the hash matches, and only redownload if it
      -- does not. 
      case exists of
        True -> do
          Stdout hash <- command [] "shasum" [out]
          case hash == shasum of
            True -> return ()
            False -> download
        False -> download 

    -- Dump all the files in the data archive to STDOUT.
    phony "list-archive-files" $ do
      need [dataArchive]
      command [] "tar" ["-tvf", dataArchive]

    -- Unpack a specific file from the data archive
    unpackDir <//> "*" %> \ out -> do
      need [dataArchive]
      putNormal $ "Unpacking '" ++ out ++ "' from archive."
      command [Cwd workDir] "tar" [ "-xf"
                                  , takeFileName dataArchive
                                  , dropDirectory1 out]

        -- dir in archive where data files are kept 
    let jsNiceDataDir     = unpackDir     </> "jsnice_data"
        -- the files with all the preprocessed training data
        trainingProcessed = jsNiceDataDir </> "training_processed" <.> "txt"
        -- the file with all the preprocessed evaluation data
        evalProcessed     = jsNiceDataDir </> "eval_processed"     <.> "txt"  

    -- Read and print training data to stdOut
    phony "read-training-data" $ do
      need [trainingProcessed]
      liftIO $ mapM_ pPrint . take 100 =<< readRawData trainingProcessed

    -- #### Collate and Separate Data #### 

        -- function to generate a particular batch set file path 
    let rawBatchSet a b = batchDir </> "rawBatchSet:" ++ a ++ "-" ++ b <.> "bin"
        -- function to generate a subsection of a batchset
        rawBatchChunk a b c = batchDir </> "rawBatchChunk:" ++ a ++ "-"
                                        ++ b ++ "-" ++ c <.> "bin"
        -- function to generate the name for a given batch
        rawBatch a b c = batchDir </> "rawBatch:" ++ a ++ "-"
                                  ++ b ++ "-" ++ c <.> "bin"
        rawBatchSetPat   = rawBatchSet "*" "*"
        rawBatchChunkPat = rawBatchChunk "*" "*" "*" 
        rawBatchPat      = rawBatch "*" "*" "*" 

    -- Split an entire input dataset randomly into divs batches and creating a
    -- set of chunks that map from batch number to a subset of data points.
    --
    -- TODO :: Slow because it forces the read of the entire input dataset. 
    rawBatchSetPat %> \ out -> do  
      putNormal $ "Generating '" ++ out ++ "'."
      case filePattern rawBatchSetPat out of
        Just [src, divs] -> do
          
          targ <- case src of
            "train" -> return trainingProcessed
            "eval"     -> return evalProcessed
            _          -> fail "rawBatchSets can only be made from 'train' or 'eval'"
            
          need [targ]

          putNormal $ "Starting to read in '" ++ targ ++ "'." 
          rawData <- getRawData targ

          let chunks = zip [1..] $ chunk (200000) rawData
              dvs = read @Int divs
              
          chunkFiles <- forP chunks $ \ (chunkNum, dat) -> do
            let chunkName = rawBatchChunk src divs (show chunkNum)
                batchMap  = makeBatches dvs dat
            putNormal $ "Generating batch chunk '" ++ chunkName ++ "'."
            liftIO $  BS.writeFile chunkName . GZ.compress . B.encode $ batchMap
            putNormal $ "Finished batch chunk '" ++ chunkName ++ "'."
            return chunkName

          putNormal $ "Finished chunks for '" ++ out ++ "'."
          liftIO $  BS.writeFile out . GZ.compress . B.encode $ chunkFiles
          
        Nothing -> fail $ "Failed to match '" ++ out ++ "' with pattern '"
                        ++ rawBatchSetPat ++ "'."
        _ -> fail "Unreachable"

    -- Create a randomly sampled batch from an input by going through all the
    -- chunks for that batch and gathering the elements for the given batch. 
    rawBatchPat %> \ out -> do
      putNormal $ "Generating batch '" ++ out ++ "'."
      case filePattern rawBatchPat out of
        Just [src, divs, batNum] -> do
          let bn = read @Int batNum

          chunks <- getRawBatchSet $ rawBatchSet src divs

          batch <- forP chunks $ \ chunkFile -> do
            chunkMap <- getRawBatchChunk chunkFile
            putNormal $ "Generating batch chunk '" ++ chunkFile ++ "'."
            return $ chunkMap ! bn

          putNormal $ "Writing batch '" ++ out ++ "'."
          liftIO $  BS.writeFile out . GZ.compress . B.encode . concat $ batch
          putNormal $ "Finished generating batch '" ++ out ++ "'."

        Nothing -> fail $ "Failed to match '" ++ out ++ "' with pattern '"
                        ++ rawBatchPat ++ "'."
        _ -> fail "Unreachable"

    -- #### Converting elements to symbols ####
    
    let batchSymbs   a b c = batchDir </> "batchSymbs:" ++ a ++ "-"
                                      ++ b ++ "-" ++ c <.> "bin"
        batchSymbsPat = batchSymbs "*" "*" "*"
        batchIDs     a b c = batchDir </> "batchIDs:" ++ a ++ "-"
                                      ++ b ++ "-" ++ c <.> "bin"
        batchIDsPat = batchIDs "*" "*" "*"
        batchSymbMap a b c = batchDir </> "batchSymbMap:" ++ a ++ "-"
                                      ++ b ++ "-" ++ c <.> "bin"
        batchSymbMapPat = batchSymbMap "*" "*" "*"
        batchIDMap   a b c = batchDir </> "batchIDMap:" ++ a ++ "-"
                                      ++ b ++ "-" ++ c <.> "bin"
        batchIDMapPat = batchIDMap "*" "*" "*"
        convertedBatch a b c = batchDir </> "convBatch:" ++ a ++ "-"
                                       ++ b ++ "-" ++ c <.> "bin"
        convertedBatchPat = convertedBatch "*" "*" "*" 
       
    -- Gather a set of all available symbols from a list of batches. 
    batchSymbsPat %> \ out -> do
      putNormal $ "Generating batch symbs '" ++ out ++ "'."
      case filePattern batchSymbsPat out of
        Just [src, divs, bats] -> do
          symbs <- case read @[Int] bats of

            -- Only a single batch, we do this normally 
            bat:[] -> do
              batch <- getRawBatch $ rawBatch src divs (show bat)
              return $ gatherAllSymbols batch
              
            -- pulling from multiple batches so we just aggregate their
            -- elements. 
            ls -> Map.unionsWith (+) <$> forP ls (\ symBat ->
                    getBatchSymbs $ batchSymbs src divs (show @[Int] [symBat]))
               
          -- Write out the set of symbols 
          putNormal $ "Writing symbol-set '" ++ out ++ "'."
          liftIO $  BS.writeFile out . GZ.compress . B.encode $ symbs
          putNormal $ "Finished generating symbol-set '" ++ out ++ "'."
                
        Nothing -> fail $ "Failed to match '" ++ out ++ "' with pattern '"
                        ++ rawBatchPat ++ "'."
        _ -> fail "Unreachable"

    -- Generate ID Mappings for all the symbols we use 
    [batchSymbMapPat, batchIDMapPat] &%> \ [sOut,iOut] -> do
      let sParse = filePattern batchSymbMapPat sOut
          iParse = filePattern batchIDMapPat iOut

      when (sParse /= iParse) $ fail $ "Outputs '" ++ sOut ++ "' and '" ++ iOut
         ++"' fail to have matching parameters."
         
      case sParse of
        Just [src, divs, bats] -> do
           
          syms <- getBatchSymbs $ batchSymbs src divs bats
          let (sMap, iMap) = buildIDMaps (Map.keysSet syms)

          -- Write out the symbMap and idMap
          putNormal $ "Writing symbol map '" ++ sOut ++ "'."
          liftIO $  BS.writeFile sOut . GZ.compress . B.encode $ sMap
          putNormal $ "Finished generating symbol map '" ++ sOut ++ "'."

          putNormal $ "Writing ID map '" ++ iOut ++ "'."
          liftIO $  BS.writeFile iOut . GZ.compress . B.encode $ iMap
          putNormal $ "Finished generating ID map '" ++ iOut ++ "'."
          
          
        Nothing -> fail $ "Failed to match '" ++ sOut ++ "' with pattern '"
                        ++ batchSymbMapPat ++ "' or '" ++ iOut ++ "' with '"
                        ++ batchIDMapPat ++ "'."
        _ -> fail "Unreachable"
          
    -- Generate the set of IDs for a particular batchSet
    batchIDsPat %> \ out -> do
      case filePattern batchIDsPat out of
        Just [src,divs,bats] -> do
          ids <- IntSet.fromAscList . IntMap.keys <$> (getBatchIDMap $
                   batchIDMap src divs bats)
                   
          putNormal $ "Writing ID set '" ++ out ++ "'."
          liftIO $  BS.writeFile out . GZ.compress . B.encode $ ids
          putNormal $ "Finished generating ID set'" ++ out ++ "'."
          
        Nothing -> fail $ "Failed to match '" ++ out ++ "' with pattern '"
                        ++ batchIDsPat ++ "'."
        _ -> fail "Unreachable"

    -- Generate a converted batch
    convertedBatchPat %> \ out -> do
      case filePattern convertedBatchPat out of
        Just [src, divs, bats] -> do
          let bts = read @[Int] bats

          sMap <- getBatchSymbsMap $ batchSymbMap src divs bats

          -- Grab each batch we're working with and replace things based on the
          -- symbol map. 
          cBat <- catMaybes . concat <$> (forP bts $ \ bat -> do
            batRaw <- getRawBatch $ rawBatch src divs (show bat)
            return $ traverse (\ k -> Map.lookup k sMap) <$> batRaw)

          putNormal $ "Writing converted batch '" ++ out ++ "'."
          liftIO $  BS.writeFile out . GZ.compress . B.encode $ cBat
          putNormal $ "Finished generating converted batch '" ++ out ++ "'."


 

    return ()
