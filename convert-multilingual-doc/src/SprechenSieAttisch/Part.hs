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

partHeader :: Maybe String -> Document -> Document
partHeader indexstr content = concat [ "\\switchcolumn[0]*[{\\part"
                            , optarg indexstr
                            , "{%\n"
                            , content
                            , "}}]\n"
                            ]
  where
    optarg Nothing = ""
    optarg (Just s) = "[{" ++ s ++ "}]"

sectionHeader :: Document -> Document
sectionHeader content = concat [ "\\switchcolumn[0]*[{\\section{%\n"
                              , content
                              , "}}]\\indent\n"
                              ]

renderFragment :: Fragment -> Document
renderFragment fg = concat ls
  where
    ls = [ modernSentences $ modern fg
         , "\\switchcolumn\n"
         , "\\begin{greek}[variant=ancient]%\n"
         , grcAlignment ++ grc fg
         , "%\n\\end{greek}%\n"
         , "\\switchcolumn*"
         , if ruleAfter fg == Just True then "[\\centering\\rule{1.5in}{1pt}]" else ""
         , "\n"
         ]
    grcAlignment = case length $ modern fg of
      0 -> ""
      1 -> ""
      _ -> "\\vspace{0.5em}\n"

renderSection :: Section -> Document
renderSection s = concat $ catMaybes [docTitle, Just docConvs]
  where
    docTitle = sectionHeader . modernSentence <$> title s
    docConvs = concatMap renderFragment $ conversations s

convertPart :: Part -> Document
convertPart p = L.intercalate "\n" $ docTitle : docSections
  where
    docTitle = partHeader (modernSentence <$> indexTitle p) . modernSentence $ part p
    docSections = map renderSection $ sections p
