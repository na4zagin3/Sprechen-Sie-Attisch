{-# LANGUAGE DeriveGeneric #-}
module SprechenSieAttisch.Lexicon where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.List as L
import Data.Maybe
import GHC.Generics

import SprechenSieAttisch.Prim

data Lexicon = Lexicon { lexicon :: ModernSentence
                       , indexTitle :: Maybe ModernSentence
                       , sections :: [Section]
                       } deriving (Generic, Show)

data Section = Section { section :: Maybe ModernSentence
                       , entries :: [Fragment]
                       } deriving (Generic, Show)

instance ToJSON Lexicon where

instance FromJSON Lexicon where

instance ToJSON Section where

instance FromJSON Section where
