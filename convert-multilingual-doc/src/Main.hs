{-# LANGUAGE DeriveGeneric #-}
module Main where

import Control.Applicative ((<$>))
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Maybe
import Data.Map (Map)
import qualified Data.Yaml as Y
import qualified Data.Map as M
import GHC.Generics
import System.Environment

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
                         , grc :: String
                         , ruleAfter :: Maybe Bool
                         } deriving (Generic, Show)

instance ToJSON Fragment where

instance FromJSON Fragment where

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

langLine :: LangCode -> String -> Document
langLine "ja" text = "\\trJA{" ++ text ++ "}"
langLine "de" text = "\\trDE{" ++ text ++ "}"
langLine "la" text = concat [ "\\trDE{%\n"
                            , "\\begin{latin}%\n"
                            , text
                            , "%\n\\end{latin}%\n"
                            ,"}"
                            ]
langLine lang text = error $ "langLine: lang: " ++ lang ++ " is not supported. text: " ++ text

langLineFrag :: LangCode -> ModernSentence -> Maybe Document
langLineFrag lang fg = langLine lang <$> lang `M.lookup` fg

modernSentence :: ModernSentence -> Document
modernSentence fg = concat $ catMaybes [textDe, textLa, textJa]
  where
    textDe = (++ "%\n") <$> langLineFrag german fg
    textLa = (++ "%\n") <$> langLineFrag latin fg
    textJa = (++ "%\n") <$> langLineFrag japanese fg

modernSentences :: [ModernSentence] -> Document
modernSentences [] = error "modernSentences: empty list"
modernSentences [m] = modernSentence m
modernSentences ms = concat ls
  where
    ss = map modernSentence ms
    ls = [ "\\begin{tabular}{lc}\n"
         , head ss
         , "& \\ldelim\\}{" ++ show (length ss) ++ "}{1em}[]\\tabularnewline\n"
         ] ++ L.intersperse "& \\tabularnewline\n" (tail ss) ++ [
           "& \\tabularnewline\n"
         , "\\end{tabular}\n"
         ]

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

renderConversation :: Fragment -> Document
renderConversation fg = concat ls
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
    docConvs = concatMap renderConversation $ conversations s

convertPart :: Part -> Document
convertPart p = L.intercalate "\n" $ docTitle : docSections
  where
    docTitle = partHeader (modernSentence <$> indexTitle p) . modernSentence $ part p
    docSections = map renderSection $ sections p

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
