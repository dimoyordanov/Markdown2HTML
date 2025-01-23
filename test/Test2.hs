module Test2 where


import Test.Hspec
import Parser (parse)
import ParserMarkdown2 (parseManyMainBlocks, MainBlocks (..), BuilderBlocks(..))
import ComposeHTML2 (transformBuilderBlock, transformHtml)

spec :: Spec
spec = do
    describe "ParseFile" $ do
        it "ParseFile none" $ do
            parse parseManyMainBlocks "" `shouldBe` Nothing -- probably should be fixed
        it "ParseFile bold" $ do
            parse parseManyMainBlocks "**3**" `shouldBe` Just [Paragraph [Bold "3"]]
        it "ParseFile bold edge" $ do
            parse parseManyMainBlocks "**3*4**" `shouldBe` Just [Paragraph [Bold "3*4"]]
        it "ParseFile bold with _" $ do
            parse parseManyMainBlocks "__3_4__" `shouldBe` Just [Paragraph [Bold "3_4"]]
        -- it "ParseFile bold" $ do
        --     parse parseManyMainBlocks "****" `shouldBe` Just [Paragraph []]
        it "ParseFile italic" $ do
            parse parseManyMainBlocks "*3*" `shouldBe` Just [Paragraph [Italic "3"]]
        it "ParseFile italic edge" $ do
            parse parseManyMainBlocks "_3_" `shouldBe` Just [Paragraph [Italic "3"]]
        it "ParseFile boldItalic" $ do
            parse parseManyMainBlocks "*** 123 ***" `shouldBe` Just [Paragraph [BoldItalic " 123 "]]
        it "ParseFile boldItalic edge" $ do
            parse parseManyMainBlocks "*** 1*2**3 ***" `shouldBe` Just [Paragraph [BoldItalic " 1*2**3 "]]
        it "ParseFile boldItalic edge2" $ do
            parse parseManyMainBlocks "___ 1_2__3 ___" `shouldBe` Just [Paragraph [BoldItalic " 1_2__3 "]]
        it "ParseFile text" $ do
            parse parseManyMainBlocks "123" `shouldBe` Just [Paragraph [Text "123"]]
        it "ParseFile text emoji" $ do
            parse parseManyMainBlocks "🤯" `shouldBe` Just [Paragraph [Text "🤯"]]
        it "ParseFile strikethrough" $ do
            parse parseManyMainBlocks "~~12~~" `shouldBe` Just [Paragraph [Strikethrough "12"]]
        it "ParseFile strikethrough edge case" $ do
            parse parseManyMainBlocks "~~1~2~~" `shouldBe` Just [Paragraph [Strikethrough "1~2"]]
        it "ParseFile link" $ do
            parse parseManyMainBlocks "[]()" `shouldBe` Just [Paragraph [Link "" ""]]
        it "ParseFile link first space" $ do
            parse parseManyMainBlocks "[a a]()" `shouldBe` Just [Paragraph [Link "a a" ""]]
        it "ParseFile link seccond space" $ do
            parse parseManyMainBlocks "[](b b)" `shouldBe` Just [Paragraph [Link "" "b b"]]
        it "ParseFile link all filled" $ do
            parse parseManyMainBlocks "[a a](b b)" `shouldBe` Just [Paragraph [Link "a a" "b b"]]
        it "ParseFile link" $ do
            parse parseManyMainBlocks "![]()" `shouldBe` Just [Paragraph [Image "" ""]]
        it "ParseFile image first space" $ do
            parse parseManyMainBlocks "![a a]()" `shouldBe` Just [Paragraph [Image "a a" ""]]
        it "ParseFile image seccond space" $ do
            parse parseManyMainBlocks "![](b b)" `shouldBe` Just [Paragraph [Image "" "b b"]]
        it "ParseFile image all filled" $ do
            parse parseManyMainBlocks "![a a](b b)" `shouldBe` Just [Paragraph [Image "a a" "b b"]]
        it "ParseFile inline math" $ do
            parse parseManyMainBlocks "$$" `shouldBe` Just [Paragraph [InlineMath ""]]
        it "ParseFile inline math simple" $ do
            parse parseManyMainBlocks "$1$" `shouldBe` Just [Paragraph [InlineMath "1"]]
        it "ParseFile inline code" $ do
            parse parseManyMainBlocks "``" `shouldBe` Just [Paragraph [InlineCode ""]]
        it "ParseFile inline code" $ do
            parse parseManyMainBlocks "`123`" `shouldBe` Just [Paragraph [InlineCode "123"]]
        it "ParseFile new line" $ do
            parse parseManyMainBlocks "\n" `shouldBe` Nothing -- probably should be fixed
        it "ParseFile bold new line" $ do
            parse parseManyMainBlocks "**3**\n" `shouldBe` Just [Paragraph [Bold "3"]]
        it "ParseFile bold edge new line" $ do
            parse parseManyMainBlocks "**3*4**\n" `shouldBe` Just [Paragraph [Bold "3*4"]]
        it "ParseFile bold with _ new line" $ do
            parse parseManyMainBlocks "__3_4__\n" `shouldBe` Just [Paragraph [Bold "3_4"]]
        -- it "ParseFile bold new line" $ do
        --     parse parseManyMainBlocks "****\n" `shouldBe` Just [Paragraph []]
        it "ParseFile italic new line" $ do
            parse parseManyMainBlocks "*3*\n" `shouldBe` Just [Paragraph [Italic "3"]]
        it "ParseFile italic edge new line" $ do
            parse parseManyMainBlocks "_3_\n" `shouldBe` Just [Paragraph [Italic "3"]]
        it "ParseFile boldItalic new line" $ do
            parse parseManyMainBlocks "*** 123 ***\n" `shouldBe` Just [Paragraph [BoldItalic " 123 "]]
        it "ParseFile boldItalic edge new line" $ do
            parse parseManyMainBlocks "*** 1*2**3 ***\n" `shouldBe` Just [Paragraph [BoldItalic " 1*2**3 "]]
        it "ParseFile boldItalic edge2 new line" $ do
            parse parseManyMainBlocks "___ 1_2__3 ___\n" `shouldBe` Just [Paragraph [BoldItalic " 1_2__3 "]]
        it "ParseFile text new line" $ do
            parse parseManyMainBlocks "123\n" `shouldBe` Just [Paragraph [Text "123"]]
        it "ParseFile text emoji new line" $ do
            parse parseManyMainBlocks "🤯\n" `shouldBe` Just [Paragraph [Text "🤯"]]
        it "ParseFile strikethrough new line" $ do
            parse parseManyMainBlocks "~~12~~\n" `shouldBe` Just [Paragraph [Strikethrough "12"]]
        it "ParseFile strikethrough edge case new line" $ do
            parse parseManyMainBlocks "~~1~2~~\n" `shouldBe` Just [Paragraph [Strikethrough "1~2"]]
        it "ParseFile link new line" $ do
            parse parseManyMainBlocks "[]()\n" `shouldBe` Just [Paragraph [Link "" ""]]
        it "ParseFile link first space new line" $ do
            parse parseManyMainBlocks "[a a]()\n" `shouldBe` Just [Paragraph [Link "a a" ""]]
        it "ParseFile link seccond space new line" $ do
            parse parseManyMainBlocks "[](b b)\n" `shouldBe` Just [Paragraph [Link "" "b b"]]
        it "ParseFile link all filled new line" $ do
            parse parseManyMainBlocks "[a a](b b)\n" `shouldBe` Just [Paragraph [Link "a a" "b b"]]
        it "ParseFile link new line" $ do
            parse parseManyMainBlocks "![]()\n" `shouldBe` Just [Paragraph [Image "" ""]]
        it "ParseFile image first space new line" $ do
            parse parseManyMainBlocks "![a a]()\n" `shouldBe` Just [Paragraph [Image "a a" ""]]
        it "ParseFile image seccond space new line" $ do
            parse parseManyMainBlocks "![](b b)\n" `shouldBe` Just [Paragraph [Image "" "b b"]]
        it "ParseFile image all filled new line" $ do
            parse parseManyMainBlocks "![a a](b b)\n" `shouldBe` Just [Paragraph [Image "a a" "b b"]]
        it "ParseFile inline math new line" $ do
            parse parseManyMainBlocks "$$\n" `shouldBe` Just [Paragraph [InlineMath ""]]
        it "ParseFile inline math simple new line" $ do
            parse parseManyMainBlocks "$1$\n" `shouldBe` Just [Paragraph [InlineMath "1"]]
        it "ParseFile inline code new line" $ do
            parse parseManyMainBlocks "``\n" `shouldBe` Just [Paragraph [InlineCode ""]]
        it "ParseFile combination simple" $ do
            parse parseManyMainBlocks "123 **123** _123_" `shouldBe` Just [Paragraph [Text "123", Text " ", Bold "123", Text " ", Italic "123"]]
        it "ParseFile combination complex" $ do
            parse parseManyMainBlocks " 2 `123`  123" `shouldBe` Just [Paragraph [Text " ", Text "2", Text " ", InlineCode "123", Text "  ", Text "123"]]
        it "ParseFile two line combination" $ do
            parse parseManyMainBlocks "123 ***123***\n**123** 9" `shouldBe` Just [Paragraph [Text "123", Text " ",BoldItalic "123"], Paragraph [Bold "123", Text " ", Text "9"]]
        it "ParseFile combination simple new line" $ do
            parse parseManyMainBlocks "123 **123** _123_\n" `shouldBe` Just [Paragraph [Text "123", Text " ", Bold "123", Text " ", Italic "123"]]
        it "ParseFile combination complex new line" $ do
            parse parseManyMainBlocks " 2 `123`  123\n" `shouldBe` Just [Paragraph [Text " ", Text "2", Text " ", InlineCode "123", Text "  ", Text "123"]]
        it "ParseFile two line combination new line" $ do
            parse parseManyMainBlocks "123 ***123***\n**123** 9\n" `shouldBe` Just [Paragraph [Text "123", Text " ",BoldItalic "123"], Paragraph [Bold "123", Text " ", Text "9"]]
        it "ParseFile header1" $ do
            parse parseManyMainBlocks "# 123" `shouldBe` Just [Header1 [Text "123"]]
        it "ParseFile header2" $ do
            parse parseManyMainBlocks "## 123" `shouldBe` Just [Header2 [Text "123"]]
        it "ParseFile header3" $ do
            parse parseManyMainBlocks "### 123" `shouldBe` Just [Header3 [Text "123"]]
        it "ParseFile header4" $ do
            parse parseManyMainBlocks "#### 123" `shouldBe` Just [Header4 [Text "123"]]
        it "ParseFile header5" $ do
            parse parseManyMainBlocks "##### 3n" `shouldBe` Just [Header5 [ Text "3n"]]
        it "ParseFile header1" $ do
            parse parseManyMainBlocks "# 123\n" `shouldBe` Just [Header1 [Text "123"]]
        it "ParseFile header2" $ do
            parse parseManyMainBlocks "## 123\n" `shouldBe` Just [Header2 [Text "123"]]
        it "ParseFile header3" $ do
            parse parseManyMainBlocks "### 123\n" `shouldBe` Just [Header3 [Text "123"]]
        it "ParseFile header4" $ do
            parse parseManyMainBlocks "#### 123\n" `shouldBe` Just [Header4 [Text "123"]]
        it "ParseFile header5" $ do
            parse parseManyMainBlocks "##### 3n\n" `shouldBe` Just [Header5 [ Text "3n"]]
        it "ParseFile header1 combination" $ do
            parse parseManyMainBlocks "# 123 ~~4n~~\n" `shouldBe` Just [Header1 [Text "123", Text " ", Strikethrough "4n"]]
        it "ParseFile header2 combination" $ do
            parse parseManyMainBlocks "## 123 ~~4n~~\n" `shouldBe` Just [Header2 [Text "123", Text " ", Strikethrough "4n"]]
        it "ParseFile header3 combination" $ do
            parse parseManyMainBlocks "### 123 ~~4n~~\n" `shouldBe` Just [Header3 [Text "123", Text " ", Strikethrough "4n"]]
        it "ParseFile header4 combination" $ do
            parse parseManyMainBlocks "#### 123 ~~4n~~\n" `shouldBe` Just [Header4 [Text "123", Text " ", Strikethrough "4n"]]
        it "ParseFile header5 combination" $ do
            parse parseManyMainBlocks "##### 3n ~~4n~~\n" `shouldBe` Just [Header5 [ Text "3n", Text " ", Strikethrough "4n"]]
        it "ParseFile parse row" $ do
            parse parseManyMainBlocks "---" `shouldBe` Just [Rule]
        it "ParseFile parse row" $ do
            parse parseManyMainBlocks "------" `shouldBe` Just [Rule]
        -- it "ParseFile parse row" $ do FIX
        --     parse parseManyMainBlocks "---ss" `shouldBe` Just [Paragraph [Text "---ss"]]
        it "ParseFile ordered list" $ do
            parse parseManyMainBlocks "1. item" `shouldBe` Just [OrderedList[[Text "item"]]]
        it "ParseFile ordered list other sign" $ do
            parse parseManyMainBlocks "1) item" `shouldBe` Just [OrderedList[[Text "item"]]]
        it "ParseFile ordered list new line" $ do
            parse parseManyMainBlocks "1. item\n" `shouldBe` Just [OrderedList[[Text "item"]]]
        it "ParseFile ordered list new line other sign" $ do
            parse parseManyMainBlocks "1) item\n" `shouldBe` Just [OrderedList[[Text "item"]]]
        it "ParseFile ordered list two" $ do
            parse parseManyMainBlocks "1. item\n2. kvantor" `shouldBe` Just [OrderedList[[Text "item"], [Text "kvantor"]]]
        it "ParseFile ordered list other sign two" $ do
            parse parseManyMainBlocks "1) item\n2) kvantor2" `shouldBe` Just [OrderedList[[Text "item"], [Text "kvantor2"]]]
        it "ParseFile unordered list" $ do
            parse parseManyMainBlocks "* item" `shouldBe` Just [UnorderedList[[Text "item"]]]
        it "ParseFile unordered list other sign" $ do
            parse parseManyMainBlocks "- item" `shouldBe` Just [UnorderedList[[Text "item"]]]
        it "ParseFile unordered list new line" $ do
            parse parseManyMainBlocks "* item\n" `shouldBe` Just [UnorderedList[[Text "item"]]]
        it "ParseFile unordered list new line other sign" $ do
            parse parseManyMainBlocks "- item\n" `shouldBe` Just [UnorderedList[[Text "item"]]]
        it "ParseFile unordered list two" $ do
            parse parseManyMainBlocks "* item\n* kvantor" `shouldBe` Just [UnorderedList[[Text "item"], [Text "kvantor"]]]
        it "ParseFile unordered list other sign two" $ do
            parse parseManyMainBlocks "- item\n- kvantor2" `shouldBe` Just [UnorderedList[[Text "item"], [Text "kvantor2"]]]
        it "ParseFile parse checkbox" $ do
            parse parseManyMainBlocks "- [] Mufasa" `shouldBe` Just [Checkbox [[Text "Mufasa"]]]
        it "ParseFile parse checkbox style" $ do
            parse parseManyMainBlocks "- [] *123*" `shouldBe` Just [Checkbox [[Italic "123"]]]
        it "ParseFile parse checkbox new line" $ do
            parse parseManyMainBlocks "- [] Mufasa\n" `shouldBe` Just [Checkbox [[Text "Mufasa"]]]
        it "ParseFile parse checkbox new line style" $ do
            parse parseManyMainBlocks "- [] *123*\n" `shouldBe` Just [Checkbox [[Italic "123"]]]
        it "ParseFile parse checkbox 2 lines" $ do
            parse parseManyMainBlocks "- [] Mufasa\n- [] Lion" `shouldBe` Just [Checkbox [[Text "Mufasa"], [Text "Lion"]]]
        it "ParseFile parse checkbox 2 lines style" $ do
            parse parseManyMainBlocks "- [] *123*\n- [] BAz" `shouldBe` Just [Checkbox [[Italic "123"], [Text "BAz"]]]
        it "ParseFile parse blockquote" $ do
            parse parseManyMainBlocks "> 123" `shouldBe` Just [BlockQuote [[Text "123"]]]
        it "ParseFile parse blockquote new line" $ do
            parse parseManyMainBlocks "> 123\n" `shouldBe` Just [BlockQuote [[Text "123"]]]
        it "ParseFile parse blockquote 1+1 lines" $ do
            parse parseManyMainBlocks "```baz\ntin\n```" `shouldBe` Just [InlineBlock "baz" "tin\n"]
        it "ParseFile parse code 1 lines" $ do
            parse parseManyMainBlocks "```baz\n```" `shouldBe` Just [InlineBlock "baz" ""]
        it "ParseFile parse code 0+1 lines" $ do
            parse parseManyMainBlocks "```\ntin\n```" `shouldBe` Just [InlineBlock "" "tin\n"]
        it "ParseFile parse code 1+2 lines" $ do
            parse parseManyMainBlocks "```baz\ntin\nmin\n```" `shouldBe` Just [InlineBlock "baz" "tin\nmin\n"]
    describe "compileToHtmlBuilderBlocks" $ do
        it "Compile text" $ do
            transformBuilderBlock (Text "1") `shouldBe` "1"
        it "Compile text empty" $ do
            transformBuilderBlock (Text " ") `shouldBe` " "
        it "Compile bold bold" $ do
            transformBuilderBlock (Bold "1") `shouldBe` "<b>1</b>"
        it "Compile text italic" $ do
            transformBuilderBlock (Italic "1") `shouldBe` "<i>1</i>"
        it "Compile text strikethrough" $ do
            transformBuilderBlock (Strikethrough "1") `shouldBe` "<s>1</s>"
        it "Compile text bolditalic" $ do
            transformBuilderBlock (BoldItalic "1") `shouldBe` "<b><i>1</i></b>"
        it "Compile text inline code" $ do
            transformBuilderBlock (InlineCode "1") `shouldBe` "<code>1</code>"
        it "Compile text inline math" $ do
            transformBuilderBlock (InlineMath "1") `shouldBe` "$1$"
        it "Compile text image" $ do
            transformBuilderBlock (Image "2" "3") `shouldBe` "<img src = \"3\" alt = \"2\"></img>"
        it "Compile text" $ do
            transformBuilderBlock (Link "2" "3") `shouldBe` "<a href = \"3\">2</a>"
    describe "CompileToHtmlMainBlocks" $ do
        it "Compile paragraph simple" $ do
            transformHtml (Paragraph [Text "1"]) `shouldBe` "<p>1</p>"
        it "Compile header1 simple" $ do
            transformHtml (Header1 [Text "1"]) `shouldBe` "<h1>1</h1>"
        it "Compile header1 complex" $ do
            transformHtml (Header1 [Text "1", Bold "3"]) `shouldBe` "<h1>1<b>3</b></h1>"
        it "Compile header1 empty" $ do
            transformHtml (Header1 []) `shouldBe` ""
        it "Compile header2 simple" $ do
            transformHtml (Header2 [Text "1"]) `shouldBe` "<h2>1</h2>"
        it "Compile header2 complex" $ do
            transformHtml (Header2 [Text "1", Bold "3"]) `shouldBe` "<h2>1<b>3</b></h2>"
        it "Compile header2 empty" $ do
            transformHtml (Header2 []) `shouldBe` ""
        it "Compile header3 simple" $ do
            transformHtml (Header3 [Text "1"]) `shouldBe` "<h3>1</h3>"
        it "Compile header3 complex" $ do
            transformHtml (Header3 [Text "1", Bold "3"]) `shouldBe` "<h3>1<b>3</b></h3>"
        it "Compile header3 empty" $ do
            transformHtml (Header3 []) `shouldBe` ""
        it "Compile header4 simple" $ do
            transformHtml (Header4 [Text "1"]) `shouldBe` "<h4>1</h4>"
        it "Compile header4 complex" $ do
            transformHtml (Header4 [Text "1", Bold "3"]) `shouldBe` "<h4>1<b>3</b></h4>"
        it "Compile header4 empty" $ do
            transformHtml (Header4 []) `shouldBe` ""
        it "Compile header5 simple" $ do
            transformHtml (Header5 [Text "1"]) `shouldBe` "<h5>1</h5>"
        it "Compile header5 complex" $ do
            transformHtml (Header5 [Text "1", Bold "3"]) `shouldBe` "<h5>1<b>3</b></h5>"
        it "Compile header5 empty" $ do
            transformHtml (Header5 []) `shouldBe` ""
        it "Compile code block empty" $ do
            transformHtml (InlineBlock "" "3") `shouldBe` "<code>\n3\n</code>"
        it "Compile code block 2 string" $ do
            transformHtml (InlineBlock "baz" "3") `shouldBe` "<code>\n3\n</code>"
        it "Compile ordered list simple" $ do
            transformHtml (OrderedList [[Text "123"]]) `shouldBe` "<ol>\n<li>123</li>\n</ol>"
        it "Compile ordered list empty" $ do
            transformHtml (OrderedList []) `shouldBe` ""
        it "Compile ordered list two" $ do
            transformHtml (OrderedList [[Text "123", Text "34"], [Text "12"]]) `shouldBe` "<ol>\n<li>12334</li>\n<li>12</li>\n</ol>"
        it "Compile unordered list simple" $ do
            transformHtml (UnorderedList [[Text "123"]]) `shouldBe` "<ul>\n<li>123</li>\n</ul>"
        it "Compile unordered list empty" $ do
            transformHtml (UnorderedList []) `shouldBe` ""
        it "Compile unordered list two" $ do
            transformHtml (UnorderedList [[Text "123", Text "34"], [Text "12"]]) `shouldBe` "<ul>\n<li>12334</li>\n<li>12</li>\n</ul>"
        it "Compile blockqoute list simple" $ do
            transformHtml (BlockQuote [[Text "123"]]) `shouldBe` "<blockquote>\n123\n</blockquote>"
        it "Compile blockqoute list empty" $ do
            transformHtml (BlockQuote []) `shouldBe` ""
        it "Compile blockqoute list two" $ do
            transformHtml (BlockQuote [[Text "123", Text "34"], [Text "12"]]) `shouldBe` "<blockquote>\n12334\n12\n</blockquote>"
