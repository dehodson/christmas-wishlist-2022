{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Questionnaire where

import GHC.Generics
import Data.Yaml

data Item = Item { link :: Maybe String
                 , item :: String
                 } deriving (Eq, Show, Generic)

instance ToJSON Item
instance FromJSON Item

data Section = Section { header :: String
                       , items :: [Item]
                       } deriving (Show, Generic)

instance ToJSON Section
instance FromJSON Section

data Questionnaire = Questionnaire { name :: String
                                   , sections :: [Section]
                                   } deriving (Show, Generic)

instance ToJSON Questionnaire
instance FromJSON Questionnaire

