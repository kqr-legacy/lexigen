{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (takeWhile, words)
import System.IO (hPutStrLn, stderr)
import Data.Ord (comparing)
import Data.Char (isSpace)
import Data.List (sort)
import Data.Monoid ((<>))
import Control.Applicative ((*>), (<*), (<|>), many, optional, liftA2)
import Control.Monad (forM_, unless, void)

import Data.Text (Text, strip, intercalate, toLower, words)
import qualified Data.Text as T (null)
import qualified Data.Text.IO as T (getContents)
import qualified Data.Text.Lazy.IO as TL (putStrLn)

import Data.Attoparsec.Text

import Text.Blaze (ToMarkup, toMarkup, preEscapedToMarkup, toValue)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (Html, toHtml, preEscapedToHtml, p, ul, li, strong, a, (!))
import Text.Blaze.Html5.Attributes (href, name)



main = do
  defs <- T.getContents
  case parseOnly dict defs of
    Right entries -> TL.putStrLn (renderHtml (htmlDict (sort entries)))
    Left err -> hPutStrLn stderr err



-- DATA TYPES

data Entry = Entry
  { _word :: Text
  , _definition :: [[RefText]]
  , _alternatives :: [(Text, [RefText])]
  } deriving Show

data RefText = Reference Text | Regular Text deriving Show

instance Eq  Entry where e1 == e2 = _word e1 == _word e2
instance Ord Entry where compare = comparing _word



-- PARSING

dict :: Parser [Entry]
dict = skipSpace *> manyTill entry endOfInput

entry :: Parser Entry
entry = do
  word <- pureText <* openBlock
  def <- many paragraph
  alts <- many alternative
  closeBlock
  return (Entry word def alts)

alternative :: Parser (Text, [RefText])
alternative = do
  name <- char '|' *> pureText <* openBlock
  desc <- option [] paragraph
  closeBlock
  return (name, desc)

openBlock :: Parser ()
openBlock = char '{' >> skipSpace

closeBlock :: Parser ()
closeBlock = char '}' >> skipSpace

paragraph :: Parser [RefText]
paragraph = liftA2 (:) linkText rest <* skipSpace
  where rest = option [] (void (optional (char '\n')) *> paragraph)

linkText :: Parser RefText
linkText = fmap Reference pureLink <|> fmap Regular pureText

pureLink :: Parser Text
pureLink = do
  takeWhile (\c -> isSpace c && c /= '\n')
  string "`{" *> pureText <* char '}'

pureText :: Parser Text
pureText = do 
  text <- fmap strip (takeWhile (notInClass "{`|}\n"))
  if T.null text then fail "empty line" else return text



-- HTML GENERATION

htmlDict :: [Entry] -> Html
htmlDict dict = ul (forM_ dict (li . htmlEntry))

htmlEntry :: Entry -> Html
htmlEntry (Entry word (first:rest) alts) = do
  a ! name (toValue (slugify word)) $ ""
  p (strong (toHtml (word <> ": ")) >> preEscapedToHtml first)
  forM_ rest (p . preEscapedToHtml)
  ul (forM_ alts htmlAlternative)

htmlAlternative :: (Text, [RefText]) -> Html
htmlAlternative (name, desc) = li . p $ do
  toHtml name
  unless (null desc) (" (" >> toHtml desc >> ")")

instance ToMarkup RefText where
  toMarkup (Reference text) = a ! href (toValue ("#" <> slugify text)) $ preEscapedToMarkup text
  toMarkup (Regular text) = preEscapedToMarkup text

instance ToMarkup a => ToMarkup [a] where
  toMarkup [] = ""
  toMarkup [x] = toMarkup x
  toMarkup (x:xs) = toMarkup x <> " " <> toMarkup xs

slugify :: Text -> Text
slugify = intercalate "-" . words . toLower

