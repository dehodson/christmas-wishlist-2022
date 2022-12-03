import Questionnaire
import Data.Yaml
import Data.Either
import Data.List
import Data.Char

getNames :: [Questionnaire] -> [String]
getNames = (name <$>)

colors :: [String]
colors = cycle [ "dark-red"
               , "dark-green"
               , "light-purple"
               , "gold"
               , "purple"
               , "orange"
               , "blue"
               ]

lowercase :: String -> String
lowercase = (toLower <$>)

indexHeading :: String
indexHeading = "<p class=\"f1 dark-red b\"><a class=\"dark-red\" href=\"https://www.youtube.com/watch?v=rjRV0G6qWgw\">Christmas Family Questionnaire 2022!</a></p>"

genIndex :: [String] -> String
genIndex = foldr eachLink indexHeading . zip colors
  where eachLink (col, p) c = c ++
                       "<p class=\"f2\"><a class=\"" ++ col ++
                       "\" href=\"./" ++
                       lowercase p ++
                       ".html\">" ++ p ++ "</a></p>\n"

itemWithLink :: Item -> String
itemWithLink i = case (link i) of
  Just l -> "<a href=\"" ++ l ++ "\">" ++ item i ++ "</a>"
  otherwise -> item i

genSection :: String -> Section -> String
genSection c s
  | items s == [] = ""
  | otherwise = "<p class=\"f2 b " ++ c ++ "\">" ++ header s ++ "</p>\n" ++
                unlines (eachItem <$> items s)
  where eachItem s = "<p class=\"f4\">" ++ (itemWithLink s) ++ "</p>"

genWishes :: Questionnaire -> String
genWishes qn = "<p class=\"f1 dark-red b\">" ++ name qn ++ "'s Wish List</p>" ++
               unlines (uncurry genSection <$> zip (drop 2 colors) (sections qn))

buildPage :: String -> String -> String -> String
buildPage head foot content = unlines [head, foot, content]

makeWishFile :: (String -> String) -> Questionnaire -> IO ()
makeWishFile pb qn = writeFile ((lowercase $ name qn) ++ ".html") (pb $ genWishes qn)

main :: IO ()
main = do
  contents <- decodeFileEither "questionnaires.yaml" :: IO (Either ParseException [Questionnaire])
  let questionnaires = fromRight [] contents
  header <- readFile "partials/head.html"
  footer <- readFile "partials/foot.html"
  let pageBuilder = buildPage header footer
  writeFile "index.html" (pageBuilder (genIndex (reverse . sort $ getNames questionnaires)))
  mapM_ (makeWishFile pageBuilder) questionnaires
