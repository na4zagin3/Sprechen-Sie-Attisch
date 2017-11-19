{-# LANGUAGE DeriveGeneric #-}
module SprechenSieAttisch.Prim where

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
                         , grc :: String
                         , ruleAfter :: Maybe Bool
                         } deriving (Generic, Show)

instance ToJSON Fragment where

instance FromJSON Fragment where

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
