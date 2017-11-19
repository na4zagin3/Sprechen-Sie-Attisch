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

renderSection :: Section -> (Maybe Document, Document)
renderSection s = (docTitle, docConvs)
  where
    docTitle = subSectionStarHeader Nothing . modernSentence <$> section s
    docConvs = concatMap renderFragment $ entries s

renderSections :: [Section] -> Document
renderSections [] = ""
renderSections (dh:ds) = concat $ p : ps
  where
    p = case renderSection dh of
      (Nothing, d) -> d
      (Just h, d) -> h ++ d
    ps = map (f . renderSection) ds
    f (t, d) = fromMaybe "\\switchcolumn[0]*[\\StarOrnament]" t ++ d

convertLexicon :: Lexicon -> Document
convertLexicon p = L.intercalate "\n" [docTitle, docSections]
  where
    docTitle = concat ["\\switchcolumn[0]*[{%\n"
                      , "\\section*{"
                      , t
                      , case it of
                          Nothing -> ""
                          Just s -> "\\addcontentsline{toc}{section}{" ++ s ++ "}"
                      , "}}]"
                      ]
    it = modernSentence <$> indexTitle p
    t = modernSentence $ lexicon p
    docSections = renderSections $ sections p

-- \section*{Altgriechische (auch neue\textrm{\textmd{{*}}} gutgebildete) Bezeichnungen
-- für moderne Begriffe aus dem Neugriechischen.\addcontentsline{toc}{section}{aus dem Neugriechischen}
