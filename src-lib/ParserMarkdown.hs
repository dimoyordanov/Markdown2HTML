{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module ParserMarkdown(parseLine) where
import Data.Char (isLetter, isDigit)
import Data.Functor (($>))
import Types (TextInformation(..))
import GHC.Base (Alternative(..))
import Parser (Parser(..))

type Predicate = (Char -> Bool)

eatPredicate :: Predicate -> Parser Char
eatPredicate b = MkParser $ \str-> case str of
                                    [] -> []
                                    (h: hs) -> ([(hs, h) | b h])

eatSpecific :: Char -> Parser Char
eatSpecific b = eatPredicate (==b)

matchesString :: String -> Parser String
matchesString [] = empty
matchesString [x] = do
    res <- eatSpecific x
    return [res]
matchesString (x: xs) = do
    val <- eatSpecific x
    res <- matchesString xs
    return $ val: res

string :: Parser String
string = some $ eatPredicate isLetter <|> eatPredicate (==' ') <|> eatPredicate isDigit

number :: Parser String
number = some $ eatPredicate isDigit

parserBold :: Parser TextInformation
parserBold = fmap ((Bold).Text)  $ (matchesString "**" >> string <* matchesString "**") <|> (matchesString "__" >> string <* matchesString "__" )

parserInline :: Parser TextInformation
parserInline = fmap Inline  $ matchesString "`" >> some (eatPredicate (/='`')) <* matchesString "`"


parserItalic :: Parser TextInformation
parserItalic = fmap ((Italic).Text)  $ (matchesString "*" >> string <* matchesString "*" ) <|> (matchesString "_" >> string <* matchesString "_" )

parserText :: Parser TextInformation
parserText = Text <$> string

parseText :: Parser [TextInformation]
parseText = many $ parserBold <|> parserItalic <|> parserText <|> parserInline

parseHeader1 :: Parser TextInformation
parseHeader1 = fmap Header  $ matchesString "# " >> parseText

parseHeader2 :: Parser TextInformation
parseHeader2 = fmap Header2  $ matchesString "## " >> parseText

parseHeader3 :: Parser TextInformation
parseHeader3 = fmap Header3  $ matchesString "### " >> parseText

parseHeader4 :: Parser TextInformation
parseHeader4 = fmap Header4  $ matchesString "#### " >> parseText

parseHeader5 :: Parser TextInformation
parseHeader5 = fmap Header5  $ matchesString "##### " >> parseText

parseBlockquote :: Parser TextInformation
parseBlockquote = fmap Blockquote  $ matchesString "> " >> parseText

parseUnordered :: Parser TextInformation
parseUnordered = fmap NonOrderedList $ (matchesString "* " >> parseText) <|> (matchesString "+ " >> parseText) <|> (matchesString "- " >> parseText)
    
parseOrdered :: Parser TextInformation
parseOrdered = fmap OrderedList $ (number >> matchesString ". " >> parseText) <|> (number >> matchesString ") " >> parseText)

parseRule :: Parser TextInformation
parseRule = eatPredicate (=='-') >> eatPredicate (=='-') >> eatPredicate (=='-') >> some (eatPredicate (=='-')) $> Rule

parseLine :: Parser TextInformation
parseLine = parseRule <|>
 parseHeader1 <|>
  parseHeader2 <|>
   parseHeader3 <|>
    parseHeader4 <|>
     parseHeader5 <|>
      parseBlockquote <|>
       parseUnordered <|>
        parseOrdered <|> 
        fmap Paragraph parseText