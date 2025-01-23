{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module ParserMarkdown2 where
import Parser (between, Parser(..), try, choice2, fallback)
import Control.Applicative (Alternative(some, empty, many, (<|>)))
import Data.List (isPrefixOf)
import Data.Char (isNumber)


data BuilderBlocks =
      Text String
    | Bold String
    | Italic String
    | BoldItalic String
    | Strikethrough String
    | Link String String
    | Image String String
    | InlineMath String
    | InlineCode String
    deriving (Show, Eq)

data MainBlocks =
      Header1 [BuilderBlocks]
    | Header2 [BuilderBlocks]
    | Header3 [BuilderBlocks]
    | Header4 [BuilderBlocks]
    | Header5 [BuilderBlocks]
    | InlineBlock String String
    | OrderedList [[BuilderBlocks]]
    | UnorderedList [[BuilderBlocks]]
    | Paragraph [BuilderBlocks]
    | BlockQuote [[BuilderBlocks]]
    | Checkbox [[BuilderBlocks]]
    | Rule
    deriving (Show, Eq)

type Predicate = (Char -> Bool)

eatPredicate :: Predicate -> Parser Char
eatPredicate b = MkParser $ \str-> case str of
                                    [] -> []
                                    (h: hs) -> ([(hs, h) | b h])

eatSpecific :: Char -> Parser Char
eatSpecific x = eatPredicate (==x)

number :: Parser Char
number = eatPredicate isNumber

matchesString :: String -> Parser String
matchesString [] = empty
matchesString [x] = do
    res <- eatSpecific x
    return [res]
matchesString (x: xs) = do
    val <- eatSpecific x
    res <- matchesString xs
    return $ val: res

eatNotString :: String -> Parser String
eatNotString [] = pure []
eatNotString [x] = do
    l <- eatPredicate (/=x)
    pure [l]
eatNotString (x: xs) = do
    l <- eatPredicate (/=x)
    y <- eatNotString xs
    pure $ l : y

parserStringIsNotIn :: [String] -> Parser String
parserStringIsNotIn [] = pure []
parserStringIsNotIn [sep] = MkParser $ \input ->
  case splitAtFirst sep input of
    Just (beforeSep, afterSep) ->
      [(sep ++ afterSep, beforeSep)]  -- If sep is found, return as specified
    Nothing ->
      [(input, input)]  -- If sep is not found, return the whole input
parserStringIsNotIn (sep: xs) = MkParser $ \input ->
  case splitAtFirst sep input of
    Just (beforeSep, afterSep) ->
      [(sep ++ afterSep, beforeSep)]  -- If sep is found, return as specified
    Nothing ->
        runParser (parserStringIsNotIn xs) input

-- Helper function to split the input string at the first occurrence of the separator
splitAtFirst :: String -> String -> Maybe (String, String)
splitAtFirst sep = go ""
  where
    go _ "" = Nothing
    go acc input@(x:xs)
      | sep `isPrefixOf` input = Just (reverse acc, drop (length sep) input)
      | otherwise = go (x:acc) xs

-- >>> maximalPossibleParse "**" $ substrings 2 "11**13123"
-- ("13123","11")

parseBoldItalic :: Parser BuilderBlocks
parseBoldItalic = BoldItalic <$> choice2 [try $ between parserBold parserBold (parserStringIsNotIn ["***", "\n"]),
               try $ between parserBold2 parserBold2 (parserStringIsNotIn ["___", "\n"])]
            where
                parserBold = matchesString "***"
                parserBold2 = matchesString "___"


parseBold :: Parser BuilderBlocks
parseBold = Bold <$> choice2 [try $ between parserBold parserBold $ parserStringIsNotIn ["**", "\n"],
                        try $ between parserBold2 parserBold2 (parserStringIsNotIn ["__", "\n"])]
            where
                parserBold = matchesString "**"
                parserBold2 = matchesString "__"

parseItalic :: Parser BuilderBlocks
parseItalic = Italic <$> choice2 [ try $ between parserBold parserBold $ parserStringIsNotIn ["*", "\n"],
                                try $ between parserBold2 parserBold2 (parserStringIsNotIn ["_", "\n"])]
            where
                parserBold = matchesString "*"
                parserBold2 = matchesString "_"

parseStrikeThrough :: Parser BuilderBlocks
parseStrikeThrough = fmap Strikethrough $ between parserBold parserBold $ parserStringIsNotIn ["~~", "\n"]
            where parserBold = matchesString "~~"

parseInlineSimple :: Parser BuilderBlocks
parseInlineSimple = fmap InlineCode $ between parserBold parserBold $ parserStringIsNotIn ["`", "\n"]
            where parserBold = matchesString "`"

parseMathSimple :: Parser BuilderBlocks
parseMathSimple = fmap InlineMath $ between parserBold parserBold $ parserStringIsNotIn ["$", "\n"]
            where parserBold = matchesString "$"

parseInlineComplex :: Parser BuilderBlocks
parseInlineComplex = fmap InlineCode $ between parserBold parserBold $ parserStringIsNotIn ["``", "\n"]
            where parserBold = matchesString "``"

parseImage :: Parser BuilderBlocks
parseImage = do
            _ <- matchesString "!["
            res1 <- parserStringIsNotIn ["]", "\n"]
            _ <- matchesString "]("
            res2 <- parserStringIsNotIn [")", "\n"]
            _ <- matchesString ")"
            return (Image res1 res2)

parseLink :: Parser BuilderBlocks
parseLink = do
            _ <- matchesString "["
            res1 <- parserStringIsNotIn ["]", "\n"]
            _ <- matchesString "]("
            res2 <- parserStringIsNotIn [")", "\n"]
            _ <- matchesString ")"
            return (Link res1 res2)


parseText :: Parser BuilderBlocks
parseText = Text <$> some (eatPredicate (not. (`elem` [' ', '\n'])))

parseBuilderBlocks :: Parser [BuilderBlocks]
parseBuilderBlocks = some $
    fallback (fallback (choice2 [
                                  try parseBoldItalic,
                                  try parseImage,
                                  try parseLink,
                                  try parseBold,
                                  try parseItalic,
                                  try parseStrikeThrough,
                                  try parseInlineComplex,
                                  try parseInlineSimple,
                                  try parseMathSimple])
             parseText)
             (fmap Text $ some $ eatPredicate (==' '))


parseHeader1 :: Parser MainBlocks
parseHeader1 = Header1 <$> (matchesString "# " >> parseBuilderBlocks)

parseHeader2 :: Parser MainBlocks
parseHeader2 = Header2 <$> (matchesString "## " >> parseBuilderBlocks)

parseHeader3 :: Parser MainBlocks
parseHeader3 = Header3 <$> (matchesString "### " >> parseBuilderBlocks)

parseHeader4 :: Parser MainBlocks
parseHeader4 = Header4 <$> (matchesString "#### " >> parseBuilderBlocks)

parseHeader5 :: Parser MainBlocks
parseHeader5 = Header5 <$> (matchesString "##### " >> parseBuilderBlocks)

parseRule :: Parser MainBlocks
parseRule = matchesString "---" >> many (eatSpecific '-') >> pure Rule

parseInlineBlock :: Parser MainBlocks
parseInlineBlock = do
    _ <- parserInlineBlock
    str1 <- many (eatPredicate (not. (`elem` ['\n']))) <* eatPredicate (const True)
    str2 <- parserStringIsNotIn ["```"]
    _ <- parserInlineBlock
    return (InlineBlock str1 str2)
        where parserInlineBlock = matchesString "```"


parseOrderedList :: Parser MainBlocks
parseOrderedList = p1 '.' <|> p1 ')'
    where
        p1 char = do
                res1 <- some number >> matchesString (char: " ") >> parseBuilderBlocks
                res2 <- many $ try (matchesString "\n" >> some number >> matchesString (char: " ") >> parseBuilderBlocks)
                return $ OrderedList $ res1:res2

parseUnorderedList :: Parser MainBlocks
parseUnorderedList =  p1 '*' <|> p1 '-'
        where
            p1 char = do
                    res <- matchesString (char:" ") >> parseBuilderBlocks
                    res2 <- many $ try (matchesString "\n" >> (matchesString (char:" ") >> parseBuilderBlocks))
                    return (UnorderedList $ res:res2)

parseBlockQuote :: Parser MainBlocks
parseBlockQuote = do
    res <- matchesString "> " >> parseBuilderBlocks
    res2 <- many $ try (matchesString "\n" >> (matchesString "> " >> parseBuilderBlocks))
    return (BlockQuote $ res:res2)

parseCheckBox :: Parser MainBlocks
parseCheckBox = do
    res <- matchesString "- [] " >> parseBuilderBlocks
    res2 <- many $ try (matchesString "\n" >> (matchesString "- [] " >> parseBuilderBlocks))
    return (Checkbox $ res:res2)

parseMainBlock :: Parser MainBlocks
parseMainBlock = fallback (choice2 [
                          try parseCheckBox,
                          try parseOrderedList,
                          try parseRule,
                          try parseUnorderedList,
                          try parseInlineBlock,
                          try parseBlockQuote,
                          try parseHeader1,
                          try parseHeader2,
                          try parseHeader3,
                          try parseHeader4,
                          try parseHeader5])
                          $ Paragraph <$> parseBuilderBlocks

parseManyMainBlocks :: Parser [MainBlocks]
parseManyMainBlocks = do
    res <- parseMainBlock
    re <- many (matchesString "\n" >> parseMainBlock)
    return $ res:re

-- >>> parse parseManyMainBlocks "3\n```eee\n\n\n\n\\n```"
-- Just [Paragraph [Text "3"],InlineBlock "eee" "\n\n\n\\n"]

-- >>> parse parseManyMainBlocks "\n*33*\n# 123\n* 1\n* 2\n3\n"
-- Just [Paragraph [],Paragraph [Italic "33"],Header1 [Text "123"],UnorderedList [[Text "1"],[Text "2"]],Paragraph [Text "3"],Paragraph []]
