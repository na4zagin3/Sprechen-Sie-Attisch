{-# LANGUAGE DeriveGeneric #-}
module SprechenSieAttisch.Part where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.List as L
import Data.Maybe
import GHC.Generics

import SprechenSieAttisch.Prim

data Section = Section { title :: Maybe ModernSentence
                       , conversations :: [Fragment]
                       } deriving (Generic, Show)

instance ToJSON Section where

instance FromJSON Section where

data Part = Part { part :: ModernSentence
                 , indexTitle :: Maybe ModernSentence
                 , sections :: [Section]
                 } deriving (Generic, Show)

instance ToJSON Part where

instance FromJSON Part where

renderSection :: Section -> Document
renderSection s = concat $ catMaybes [docTitle, Just docConvs]
  where
    docTitle = sectionHeader Nothing . modernSentence <$> title s
    docConvs = concatMap renderFragment $ conversations s

convertPart :: Part -> Document
convertPart p = L.intercalate "\n" $ docTitle : docSections
  where
    docTitle = partHeader (modernSentence <$> indexTitle p) . modernSentence $ part p
    docSections = map renderSection $ sections p
