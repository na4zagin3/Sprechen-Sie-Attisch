{-# LANGUAGE DeriveGeneric #-}
module SprechenSieAttisch.Prim where

import Control.Arrow (second)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.List as L
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import GHC.Generics

type LangCode = String

greek :: LangCode
greek = "grc"

german :: LangCode
german = "de"

japanese :: LangCode
japanese = "ja"

latin :: LangCode
latin = "la"

type Document = String

type ModernSentence = Map LangCode String

data Fragment = Fragment { modern :: [ModernSentence]
                         , grc :: [String]
                         , ruleAfter :: Maybe Bool
                         } deriving (Generic, Show)

instance ToJSON Fragment where

instance FromJSON Fragment where

langLine :: LangCode -> String -> Document
langLine "ja" text = "\\trJA{" ++ text ++ "}"
langLine "de" text = "\\trDE{" ++ text ++ "}"
langLine "la" text = concat [ "\\trDE{%\n" -- ToDo: This is redundant
                            , "\\begin{latin}%\n"
                            , text
                            , "%\n\\end{latin}%\n"
                            ,"}"
                            ]
langLine "grc" text = concat [ "\\begin{greek}[variant=ancient]%\n"
                             , text
                             , "%\n\\end{greek}%\n"
                             ]
langLine lang text = error $ "langLine: lang: " ++ lang ++ " is not supported. text: " ++ text

langLineFrag :: LangCode -> ModernSentence -> Maybe Document
langLineFrag lang fg = langLine lang <$> lang `M.lookup` fg

modernSentence :: ModernSentence -> [(LangCode, Document)]
modernSentence fg = catMaybes [textDe, textLa, textJa]
  where
    textDe = (\x -> (german, x)) . (++ "%\n") <$> langLineFrag german fg
    textLa = (\x -> (latin, x)) . (++ "%\n") <$> langLineFrag latin fg
    textJa = (\x -> (japanese, x)) . (++ "%\n") <$> langLineFrag japanese fg

modernSentences :: [ModernSentence] -> [(LangCode, Document)]
modernSentences [] = error "modernSentences: empty list"
modernSentences [m] = modernSentence m
modernSentences ms = modernSentence tableSentence
  where
    tableSentence = M.map renderTable $ M.unionsWith (<>) $ map (M.map L.singleton) ms
    renderTable ss = concat $ [ "\\begin{tabular}{lc}\n"
                   , head ss
                   , "& \\ldelim\\}{" ++ show (length ss) ++ "}{1em}[]\\tabularnewline\n"
                   ] ++ L.intersperse "& \\tabularnewline\n" (tail ss) ++ [
                     "& \\tabularnewline\n"
                   , "\\end{tabular}\n"
                   ]

leftBraced :: [Document] -> Document
leftBraced [] = error "leftBraced: empty list"
leftBraced [d] = d
leftBraced ms = concat ls
  where
    ls = [ "\\begin{tabular}{ll}\n"
         , "\\ldelim\\{{" ++ show (length ms) ++ "}{1em}[] & "
         , head ms
         , "\\tabularnewline\n& "
         ] ++ L.intersperse "\\tabularnewline\n& " (tail ms) ++ [
           "\\tabularnewline\n"
         , "\\end{tabular}\n"
         ]

header :: String -> Maybe String -> Document -> Document
header headerCmd indexstr content = concat strs
  where
    optarg Nothing = ""
    optarg (Just s) = "[{" ++ s ++ "}]"
    strs = [ "\\switchcolumn[0]*[{"
           , headerCmd
           , optarg indexstr
           , "{%\n"
           , content
           , "}}]\n"
           ]

renderFragment :: Fragment -> Document
renderFragment fg = concat ls
  where
    ls = [ concat $ map renderModernSentence $ modernSentences $ modern fg
         , grcAlignment
         , leftBraced . map (langLine greek) $ grc fg
         , "\\switchcolumn*"
         , if ruleAfter fg == Just True then "[\\centering\\rule{1.5in}{1pt}]" else ""
         , "\n"
         ]
    renderModernSentence (lang, ms) = concat [ modernAlignment
                                          , ms
                                          , "\\switchcolumn"
                                          , lang
                                          , "\n"
                                          ]
    grcAlignment = alignment . length $ modern fg
    modernAlignment = alignment . length $ grc fg
    alignment n = if n > 1 then "\\vspace{0.5em}\n" else ""

partHeader :: Maybe String -> Document -> Document
partHeader = header "\\part"

sectionHeader :: Maybe String -> Document -> Document
sectionHeader = header "\\section"

subSectionHeader :: Maybe String -> Document -> Document
subSectionHeader = header "\\subsection"

subSectionStarHeader :: Maybe String -> Document -> Document
subSectionStarHeader = header "\\subsection*"
