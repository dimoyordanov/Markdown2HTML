module Main (main) where

import Test.Hspec
import Parser ( parse)
import Types (TextInformation(..))
import ParserMarkdown (parseLine)
import ComposeHtml (transformHtml2)

spec :: Spec
spec = do
    describe "parseLine" $ do
        it "ParseLine none" $ do
            (parse parseLine "") `shouldBe` Just (Paragraph [])
        it "ParseLine bold" $ do
            (parse parseLine "**3**") `shouldBe` Just (Paragraph [Bold (Text "3")])
        it "ParseLine italic" $ do
            (parse parseLine "*3*") `shouldBe` Just (Paragraph [Italic (Text "3")])
        it "ParseLine bold2" $ do
            (parse parseLine "__3__") `shouldBe` Just (Paragraph [Bold (Text "3")])
        it "ParseLine italic2" $ do
            (parse parseLine "_3_") `shouldBe` Just (Paragraph [Italic (Text "3")])
        it "ParseLine bold2 none" $ do
            (parse parseLine "____") `shouldBe` Just (Paragraph [])
        it "ParseLine italic2 nono" $ do
            (parse parseLine "__") `shouldBe` Just (Paragraph [])
        it "ParseLine inline" $ do
            (parse parseLine "`hello`") `shouldBe` Just (Paragraph [Inline "hello"])
        it "ParseLine inline none" $ do
            (parse parseLine "``") `shouldBe` Just (Paragraph [])
        it "ParseLine header" $ do
            (parse parseLine "# hello") `shouldBe` Just (Header [Text "hello"])
        it "ParseLine header" $ do
            (parse parseLine "# ") `shouldBe` Just (Header [])
        it "ParseLine header2" $ do
            (parse parseLine "## hello from 2") `shouldBe` Just (Header2 [Text "hello from 2"])
        it "ParseLine blockquote" $ do
            (parse parseLine "> thank") `shouldBe` Just (Blockquote [Text "thank"])
        it "ParseLine Unordered list" $ do
            (parse parseLine "* il") `shouldBe` Just (NonOrderedList [Text "il"])
        it "ParseLine Unordered list2" $ do
            (parse parseLine "+ il") `shouldBe` Just (NonOrderedList [Text "il"])
        it "ParseLine Unordered list3" $ do
            (parse parseLine "- il") `shouldBe` Just (NonOrderedList [Text "il"])
        it "ParseLine Ordered list2" $ do
            (parse parseLine "1. il") `shouldBe` Just (OrderedList [Text "il"])
        it "ParseLine Ordered list3" $ do
            (parse parseLine "1) il") `shouldBe` Just (OrderedList [Text "il"])
        it "ParseLine Ordered list2++" $ do
            (parse parseLine "1. __il__") `shouldBe` Just (OrderedList [Bold $ Text "il"])
        it "ParseLine Ordered list3++" $ do
            (parse parseLine "1) mil __il__ vil") `shouldBe` Just (OrderedList [Text "mil ", Bold $ Text "il", Text " vil"])
        it "ParseLine line" $ do
            (parse parseLine "----") `shouldBe` Just (Rule)
    
    describe "Html composer" $ do
        it "Compose empty" $ do
            (transformHtml2 []) `shouldBe` ""
        it "Compose empty" $ do
            (transformHtml2 [Rule]) `shouldBe` "\n<hr>\n"
        it "Compose text" $ do
            (transformHtml2 [Text "Bazinga"]) `shouldBe` "Bazinga"
        it "Compose italic" $ do
            (transformHtml2 [Italic $ Text "Bazinga"]) `shouldBe` "<i>Bazinga</i>"
        it "Compose bold" $ do
            (transformHtml2 [Bold $ Text "Bazinga"]) `shouldBe` "<b>Bazinga</b>"
        it "Compose inline" $ do
            (transformHtml2 [Inline $ "Bazinga"]) `shouldBe` "<code>Bazinga</code>"
        it "Compose paragraph" $ do
            (transformHtml2 [Paragraph [Text "123"]]) `shouldBe` "<p>123</p>\n"
        it "Compose header1" $ do
            (transformHtml2 [Header [Text "123"]]) `shouldBe` "<h1>123</h1>\n"
        it "Compose header2" $ do
            (transformHtml2 [Header2 [Text "123"]]) `shouldBe` "<h2>123</h2>\n"
        it "Compose header2 complex" $ do
            (transformHtml2 [Header2 [Bold $ Text $ "123", Text $ "321"]]) `shouldBe` "<h2><b>123</b>321</h2>\n"
        it "Compose header2 none" $ do
            (transformHtml2 [Header2 []]) `shouldBe` "<h2></h2>\n"
        it "Compose header2 Blockquote" $ do
            (transformHtml2 [Blockquote [Text "eho"], Blockquote [Text "beho"]]) `shouldBe` "<blockquote>\neho beho \n</blockquote>\n"
        it "Compose header2 orderedList" $ do
            (transformHtml2 [NonOrderedList [Text "eho"], NonOrderedList [Text "beho"]]) `shouldBe` "<ul>\n<li>eho</li>\n<li>beho</li>\n</ul>\n"
        it "Compose header2 unorderedList" $ do
            (transformHtml2 [OrderedList [Text "eho"], OrderedList [Text "beho"]]) `shouldBe` "<ol>\n<li>eho</li>\n<li>beho</li>\n</ol>\n"


main :: IO ()
main = hspec $ do
    spec