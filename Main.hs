{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, takeWhile, null, length)
import Data.Monoid ((<>))
import Control.Applicative ((*>), (<*), (<$>), many)
import Control.Monad (forM_, unless)
import System.Environment (getArgs)

import Data.List.Split (splitOn)
import Data.Text (Text, strip, null, intercalate, length)
import Data.Text.IO (readFile)
import qualified Data.Text.Lazy.IO as T

import Data.Attoparsec.Text

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, toHtml, preEscapedToHtml, p, ul, li, strong)


main = do
  [fn] <- getArgs
  defs <- readFile fn
  case parseOnly dict defs of
    Right dict -> T.putStrLn (renderHtml (htmlDict dict))
    Left err -> putStrLn err



-- DATA TYPES

data Entry = Entry
  { word :: Text
  , definition :: [Text]
  , alternatives :: [(Text, Text)]
  } deriving Show



-- PARSING

dict :: Parser [Entry]
dict = manyTill (skipSpace *> entry <* skipSpace) endOfInput

entry :: Parser Entry
entry = do
  word <- textLine <* char '{'
  def <- paragraphs
  alts <- many alternative <* char '}'
  skipSpace
  return (Entry (strip word) def alts)

alternative :: Parser (Text, Text)
alternative = do
  name <- char '|' *> textLine <* char '{' <* skipSpace
  desc <- paragraphs <* skipSpace <* char '}'
  skipSpace
  return (strip name, intercalate "\n" desc)

paragraphs :: Parser [Text]
paragraphs = toParagraphs . map strip <$> sepBy textLine (char '\n')
  where
    toParagraphs = map (intercalate "\n") . filter hasContent . splitOn [""]
    hasContent group = sum (map length group) > 0

textLine :: Parser Text
textLine = takeWhile (notInClass "{|}\n")



-- HTML GENERATION

htmlDict :: [Entry] -> Html
htmlDict dict = ul (forM_ dict (li . htmlEntry))

htmlEntry :: Entry -> Html
htmlEntry (Entry word (first:rest) alts) = do
  p (strong (toHtml (word <> ": ")) >> preEscapedToHtml first)
  forM_ rest (p . preEscapedToHtml)
  ul (htmlAlternatives alts)

htmlAlternatives :: [(Text, Text)] -> Html
htmlAlternatives alts =
  forM_ alts htmlAlternative

htmlAlternative :: (Text, Text) -> Html
htmlAlternative (name, desc) = li . p . toHtml $
  if null desc
     then name
     else name <> " (" <> desc <> ")"
