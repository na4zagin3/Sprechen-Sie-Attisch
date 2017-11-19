{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import qualified Data.Yaml as Y
import System.Environment

import SprechenSieAttisch.Part (convertPart)

fileConvertNormalPart :: FilePath -> FilePath -> IO ()
fileConvertNormalPart outfile infile = do
  p <- either error id . Y.decodeEither <$> BS.readFile infile
  writeFile outfile $ convertPart p

execute :: [String] -> IO ()
execute ("--part-to-file":outfile:infile:args) = fileConvertNormalPart outfile infile >> execute args
execute [] = return ()
execute args = error $ "Unknown arguments: " ++ show args

main :: IO ()
main = getArgs >>= execute
