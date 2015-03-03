{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, takeWhile, null, length, lines, unlines)
import Data.Ord (comparing)
import Data.List (sort, groupBy)
import Data.Monoid ((<>))
import Control.Applicative ((*>), (<*), (<$>), many)
import Control.Monad (forM_, unless)
import System.Environment (getArgs)

import Data.Text (Text, strip, null, intercalate, length, lines, unlines, splitOn)
import Data.Text.IO (readFile)
import qualified Data.Text.Lazy.IO as T

import Data.Attoparsec.Text

import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, toHtml, preEscapedToHtml, p, ul, li, strong)


main = do
  [fn] <- getArgs
  defs <- readFile fn
  case parseOnly dict defs of
    Right dict -> T.putStrLn (renderHtml (htmlDict (sort dict)))
    Left err -> putStrLn err



-- DATA TYPES

data Entry = Entry
  { _word :: Text
  , _definition :: [Text]
  , _alternatives :: [(Text, Text)]
  } deriving Show

type RefText = [Either Text Text]

instance Eq  Entry where e1 == e2 = _word e1 == _word e2
instance Ord Entry where compare = comparing _word


-- PARSING

dict :: Parser [Entry]
dict = manyTill (skipSpace *> entry <* skipSpace) endOfInput

entry :: Parser Entry
entry = do
  word <- fmap normalise (pureText <* char '{')
  def <- fmap (splitOn "\n\n") pureText
  alts <- many (alternative <* skipSpace) <* char '}'
  return (Entry word def alts)

alternative :: Parser (Text, Text)
alternative = do
  name <- char '|' *> pureText <* char '{'
  desc <- pureText <* char '}'
  return (name, desc)

--linkText :: Parser RefText
--linkText = many (fmap Left link <|> fmap Right pureText)

--link :: Parser Text
--link = fmap normalise (string "`{" *> pureText <* char '}')

pureText :: Parser Text
pureText = fmap cleanedText (takeWhile (notInClass "{`|}"))
  where cleanedText = strip . unlines . compactLF . map strip . lines
        compactLF = map (!!0) . groupBy (\a b -> null (a <> b))

normalise :: Text -> Text
normalise = intercalate " " . lines


-- HTML GENERATION

htmlDict :: [Entry] -> Html
htmlDict dict = ul (forM_ dict (li . htmlEntry))

htmlEntry :: Entry -> Html
htmlEntry (Entry word (first:rest) alts) = do
  p (strong (toHtml (word <> ": ")) >> preEscapedToHtml first)
  forM_ rest (p . preEscapedToHtml)
  ul (forM_ alts htmlAlternative)

htmlAlternative :: (Text, Text) -> Html
htmlAlternative (name, desc) = li . p . toHtml $
  if null desc
     then name
     else name <> " (" <> desc <> ")"
