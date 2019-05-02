{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Applicative ((<$>))
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import System.Environment

import SprechenSieAttisch.Prim (Document)
import SprechenSieAttisch.LaTeX (convertPart, convertLexicon)

fileConvertNormalPart :: (ToJSON a, FromJSON a) => (a -> Document) -> FilePath -> FilePath -> IO ()
fileConvertNormalPart conv outfile infile = do
  p <- either error id . Y.decodeEither <$> BS.readFile infile
  writeFile outfile $ conv p

execute :: [String] -> IO ()
execute ("--part-to-file":outfile:infile:args) = fileConvertNormalPart convertPart outfile infile >> execute args
execute ("--lexicon-to-file":outfile:infile:args) = fileConvertNormalPart convertLexicon outfile infile >> execute args
execute [] = return ()
execute args = error $ "Unknown arguments: " ++ show args

main :: IO ()
main = getArgs >>= execute
