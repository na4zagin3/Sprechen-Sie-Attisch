{-# LANGUAGE DeriveGeneric #-}
module SprechenSieAttisch.LaTeX where

import Data.Aeson (ToJSON, FromJSON)
import qualified Data.List as L
import Data.Maybe
import GHC.Generics

import SprechenSieAttisch.Prim
import SprechenSieAttisch.Part as Part
import SprechenSieAttisch.Lexicon as Lexicon

renderPartSection :: Part.Section -> Document
renderPartSection s = concat $ catMaybes [docTitle, Just docConvs]
  where
    docTitle = sectionHeader Nothing . modernSentence <$> title s
    docConvs = concatMap renderFragment $ conversations s

convertPart :: Part -> Document
convertPart p = L.intercalate "\n" $ docTitle : docSections
  where
    docTitle = partHeader (modernSentence <$> Part.indexTitle p) . modernSentence $ part p
    docSections = map renderPartSection $ Part.sections p

renderLexiconSection :: Lexicon.Section -> (Maybe Document, Document)
renderLexiconSection s = (docTitle, docConvs)
  where
    docTitle = subSectionStarHeader Nothing . modernSentence <$> section s
    docConvs = concatMap renderFragment $ entries s

renderLexiconSections :: [Lexicon.Section] -> Document
renderLexiconSections [] = ""
renderLexiconSections (dh:ds) = concat $ p : ps
  where
    p = case renderLexiconSection dh of
      (Nothing, d) -> d
      (Just h, d) -> h ++ d
    ps = map (f . renderLexiconSection) ds
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
    it = modernSentence <$> Lexicon.indexTitle p
    t = modernSentence $ lexicon p
    docSections = renderLexiconSections $ Lexicon.sections p

-- \section*{Altgriechische (auch neue\textrm{\textmd{{*}}} gutgebildete) Bezeichnungen
-- für moderne Begriffe aus dem Neugriechischen.\addcontentsline{toc}{section}{aus dem Neugriechischen}
