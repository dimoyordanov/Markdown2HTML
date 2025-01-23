module Test2 where


import Test.Hspec
import Parser (parse)
import ParserMarkdown2 (parseManyMainBlocks, MainBlocks (..), BuilderBlocks(..))

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
        it "ParseFile inline code new line" $ do
            parse parseManyMainBlocks "`123`\n" `shouldBe` Just [Paragraph [InlineCode "123"]]