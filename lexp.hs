{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, takeWhile, null)
import Data.Monoid ((<>))
import Control.Applicative ((*>), (<*), many)
import Control.Monad (forM_, unless)
import System.Environment (getArgs)

import Data.Text (Text, strip, null)
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
  word <- normalText <* char '{'
  def <- sepBy normalText (satisfy (== '\n'))
  alts <- many alternative <* char '}'
  skipSpace
  return (Entry (strip word) (toParagraphs (map strip def) "" []) alts)
  where
    toParagraphs [] current all = all ++ [current]
    toParagraphs (this:next) current all =
      if null this
         then toParagraphs next "" (all ++ [current])
         else toParagraphs next (current <> "\n" <> this) all

alternative :: Parser (Text, Text)
alternative = do
  name <- char '|' *> normalText <* char '{'
  desc <- normalText <* char '}'
  skipSpace
  return (strip name, strip desc)

normalText :: Parser Text
normalText = takeWhile (notInClass "{|}\n")

paragraphBreak :: Parser ()
paragraphBreak = do
  skip (== '\n')
  skipWhile (inClass " \t")
  skip (== '\n')



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
