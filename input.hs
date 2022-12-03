import qualified Data.ByteString.Lazy as B
import Questionnaire
import GHC.Generics
import Data.Either
import Data.Yaml
import System.IO
import Data.List

sectionNames :: [String]
sectionNames = [ "Favorite Snacks &amp; Drinks"
               , "Hobbies &amp; Interests"
               , "Favorite Colors &amp; Scents"
               , "Home Items Wanted"
               , "Personal Care Items"
               , "Gifts for Pets"
               , "Tech Items Wanted"
               , "Book Wish List"
               , "Car Items Wanted"
               , "Music / Video Wish List"
               , "Favorite Restaurants"
               , "Gift Card Ideas"
               , "Desk / Office Items Wanted"
               , "Donate to ___ in my Name"
               , "I DON'T Want / Need"
               , "Misc. Gift Ideas"
               ]

promptFor :: String -> IO String
promptFor s = putStr s >> hFlush stdout >> getLine

getSection :: String -> IO Section
getSection name = do
  putStrLn name
  let loop = do {
    item <- getLine ;
    case item of
      "" -> return []
      _ -> (item :) <$> loop
  }
  items <- loop
  return $ Section name (Item Nothing <$> items)

getQuestionnaire :: String -> IO Questionnaire
getQuestionnaire name = do
  hFlush stdout
  sections <- sequenceA $ getSection <$> sectionNames
  return $ Questionnaire name sections

insertQuestionnaire :: Questionnaire -> [Questionnaire] -> [Questionnaire]
insertQuestionnaire q qs = do
  let i = findIndex (((name q) ==) . name) qs
  case i of
    Just index -> take index qs ++ [q] ++ drop (index + 1) qs
    Nothing -> q : qs

mergeQuestionnaires :: [Questionnaire] -> [Questionnaire] -> [Questionnaire]
mergeQuestionnaires = foldr insertQuestionnaire

main :: IO ()
main = do
  contents <- decodeFileEither "questionnaires.yaml" :: IO (Either ParseException [Questionnaire])
  let initialQuestionnaires = fromRight [] contents
  let loop = do {
    name <- promptFor "Person's name: " ;
    case name of
      "" -> return []
      _ -> do {
        q <- getQuestionnaire name ;
        (q :) <$> loop
      }
  }
  questionnaires <- loop
  encodeFile "questionnaires.yaml" (mergeQuestionnaires initialQuestionnaires questionnaires)
  putStrLn "Thank you! :)"
