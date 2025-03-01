module Test1 where


import Test.Hspec
import Parser ( parse)
import Types (TextInformation(..))
import ParserMarkdown (parseLine)
import qualified ComposeHtml as T (transformHtml2)

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
        it "ParseLine link" $ do
            (parse parseLine "aba [Hello](google.com)") `shouldBe` Just (Paragraph [Text "aba ",Link "Hello" "google.com"])
        it "ParseLine link" $ do
            (parse parseLine "aba ![Hello](google.com)") `shouldBe` Just (Paragraph [Text "aba ",Image "Hello" "google.com"])
        it "ParseLine link" $ do
            (parse parseLine "[Hello](google.com)") `shouldBe` Just (Paragraph [Link "Hello" "google.com"])
        it "parseLink link2" $ do
            (parse parseLine "![Markdown Logo](https://markdown-here.com/img/icon256.png)") `shouldBe`  Just (Paragraph [Image "Markdown Logo" "https://markdown-here.com/img/icon256.png"])
        it "ParseLine link" $ do
            (parse parseLine "![Hello](google.com)") `shouldBe` Just (Paragraph [Image "Hello" "google.com"])
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
        it "ParseLine checkbox" $ do
            (parse parseLine "- [] mil") `shouldBe` Just (Checkbox [Text "mil"])
        it "ParseLine line" $ do
            (parse parseLine "----") `shouldBe` Just (Rule)
    
    describe "Html composer" $ do
        it "Compose empty" $ do
            (T.transformHtml2 []) `shouldBe` ""
        it "Compose empty" $ do
            (T.transformHtml2 [Rule]) `shouldBe` "\n<hr>\n"
        it "Compose text" $ do
            (T.transformHtml2 [Text "Bazinga"]) `shouldBe` "Bazinga"
        it "Compose italic" $ do
            (T.transformHtml2 [Italic $ Text "Bazinga"]) `shouldBe` "<i>Bazinga</i>"
        it "Compose bold" $ do
            (T.transformHtml2 [Bold $ Text "Bazinga"]) `shouldBe` "<b>Bazinga</b>"
        it "Compose bold" $ do
            (T.transformHtml2 [StrikeThrough $ Text "Bazinga"]) `shouldBe` "<s>Bazinga</s>"
        it "Compose link" $ do
            (T.transformHtml2 [Link "aba" "baba"]) `shouldBe` "<a href = \"baba\">aba</a>"
        it "Compose image" $ do
            (T.transformHtml2 [Image "aba" "baba"]) `shouldBe` "<img src = \"baba\" alt = \"aba\"></img>"
        it "Compose inline" $ do
            (T.transformHtml2 [Inline $ "Bazinga"]) `shouldBe` "<code>Bazinga</code>"
        it "Compose paragraph" $ do
            (T.transformHtml2 [Paragraph [Text "123"]]) `shouldBe` "<p>123</p>\n"
        it "Compose header1" $ do
            (T.transformHtml2 [Header [Text "123"]]) `shouldBe` "<h1>123</h1>\n"
        it "Compose header2" $ do
            (T.transformHtml2 [Header2 [Text "123"]]) `shouldBe` "<h2>123</h2>\n"
        it "Compose header3" $ do
            (T.transformHtml2 [Header3 [Text "123"]]) `shouldBe` "<h3>123</h3>\n"
        it "Compose header4" $ do
            (T.transformHtml2 [Header4 [Text "123"]]) `shouldBe` "<h4>123</h4>\n"
        it "Compose header5" $ do
            (T.transformHtml2 [Header5 [Text "123"]]) `shouldBe` "<h5>123</h5>\n"
        it "Compose header2 complex" $ do
            (T.transformHtml2 [Header2 [Bold $ Text $ "123", Text $ "321"]]) `shouldBe` "<h2><b>123</b>321</h2>\n"
        it "Compose header2 none" $ do
            (T.transformHtml2 [Header2 []]) `shouldBe` "<h2></h2>\n"
        it "Compose header2 Blockquote" $ do
            (T.transformHtml2 [Blockquote [Text "eho"], Blockquote [Text "beho"]]) `shouldBe` "<blockquote>\neho beho \n</blockquote>\n"
        it "Compose header2 orderedList" $ do
            (T.transformHtml2 [NonOrderedList [Text "eho"], NonOrderedList [Text "beho"]]) `shouldBe` "<ul>\n<li>eho</li>\n<li>beho</li>\n</ul>\n"
        it "Compose header2 unorderedList" $ do
            (T.transformHtml2 [OrderedList [Text "eho"], OrderedList [Text "beho"]]) `shouldBe` "<ol>\n<li>eho</li>\n<li>beho</li>\n</ol>\n"