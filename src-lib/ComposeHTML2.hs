module ComposeHTML2 where

import ParserMarkdown2 (BuilderBlocks(..), MainBlocks(..))

generatedCss :: String
generatedCss = "\
\ <script> \
\  MathJax = { \
\   tex: {inlineMath: [['$', '$'], ['\\(', '\\)']]} \
\  }; \
\  </script> \
\ <script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-chtml.js\"></script>\
\<link href=\"https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap\" rel=\"stylesheet\"> \
\ <style> \
\        body { \
\            font-family: 'Roboto', sans-serif; \
\            color: #333; \
\            margin: 0; \
\            padding: 0 20px; \
\            max-width: 800px; \
\            margin: auto; \
\            line-height: 1.6; \
\            background-color: #fafafa; \
\        } \
\        h1, h2 { \
\            font-weight: normal; \
\        } \
\        h1 { \
\            margin-top: 40px; \
\            font-size: 2.5em; \
\        } \
\        h2 { \
\            margin-top: 30px; \
\            color: #555; \
\            font-size: 1.8em; \
\        } \
\        p { \
\            margin-bottom: 20px; \
\            font-size: 1em; \
\        } \
\        hr { \
\            border: 0; \
\            border-top: 1.5px solid #eee; \
\            margin: 40px 0; \
\        } \
\        code { \
\            background-color: #f5f5f5; \
\            padding: 2px 4px; \
\            border-radius: 4px; \
\            font-family: 'Courier New', monospace; \
\        } \
\ hr {\
\            border: 0; \
\            border-top: 1px solid #eee; \
\            margin: 40px 0; \
\ } \
\        /* Допълнително стилове за минималистичен дизайн */ \
\        a { \
\            color: #3498db; \
\            text-decoration: none; \
\        } \
\        a:hover { \
\            text-decoration: underline; \
\        } \
\    </style>"


putInplaceHtml :: String -> String
putInplaceHtml arg= "<!DOCTYPE html>\n" ++
                    "<html lang=\"en\">\n" ++
                    "<head>\n"++
                    "    <meta charset=\"UTF-8\">\n"++
                    "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"++
                    "    <title>Dimos Blog</title>\n"++
                    generatedCss ++ "\n" ++
                    "</head>\n"++
                    "<body>\n" ++
                    arg ++
                    "</body>\n" ++
                    "</html>\n"

transformHtmlList:: [MainBlocks] -> String
transformHtmlList l = putInplaceHtml $ transformHtml2 $ filter (/=(Paragraph []))l

transformHtml2 :: [MainBlocks] -> String
transformHtml2 = foldr (\l r -> transformHtml l ++ ('\n':r)) ""

transformHtml :: MainBlocks -> String
transformHtml (Header1 b) = "<h1>" ++ transformBuilderBlocks b ++ "</h1>"
transformHtml (Header2 b) = "<h2>" ++ transformBuilderBlocks b ++ "</h2>"
transformHtml (Header3 b) = "<h3>" ++ transformBuilderBlocks b ++ "</h3>"
transformHtml (Header4 b) = "<h4>" ++ transformBuilderBlocks b ++ "</h4>"
transformHtml (Header5 b) = "<h5>" ++ transformBuilderBlocks b ++ "</h5>"
transformHtml (InlineBlock _ b) = "<code>\n" ++ b ++ "\n</code>"
transformHtml (Rule) = "<hr>"
transformHtml (OrderedList b) = "<ol>\n" ++ foldr (\l r -> "<li>" ++ transformBuilderBlocks l ++ "</li>" ++ ('\n':r)) "" b ++ "</ol>"
transformHtml (UnorderedList b) = "<ul>\n" ++ foldr (\l r -> "<li>" ++ transformBuilderBlocks l ++ "</li>" ++ ('\n':r)) "" b ++ "</ul>"
transformHtml (BlockQuote b) = "<blockquote>\n" ++ foldr (\l r -> transformBuilderBlocks l ++ ('\n':r)) "" b ++ "</blockquote>"
transformHtml (Paragraph b) = "<p>" ++ transformBuilderBlocks b ++ "</p>"
transformHtml (Checkbox b) = foldr (\l r -> "<div>\n<input type=\"checkbox\" id=\"scales\" name=\"scales\">\n<label for=\"scales\">" ++ transformBuilderBlocks l ++ "</label>\n</div>\n" ++ ('\n':r)) "" b

transformBuilderBlocks :: [BuilderBlocks] -> String
transformBuilderBlocks = foldr (\l r -> transformBuilderBlock l ++ r) ""

transformBuilderBlock :: BuilderBlocks -> String
transformBuilderBlock (Text t) = t
transformBuilderBlock (Bold t) = "<b>" ++t ++ "</b>"
transformBuilderBlock (Italic t) = "<b>" ++t ++ "</b>"
transformBuilderBlock (BoldItalic t) = "<b><i>" ++t ++ "</i></b>"
transformBuilderBlock (Strikethrough t) = "<s>" ++t ++ "</s>"
transformBuilderBlock (InlineCode t) = "<code>" ++t ++ "</code>"
transformBuilderBlock (InlineMath t) = "$" ++t ++ "$"
transformBuilderBlock (Image a b) = "<img src = \"" ++ b ++ "\" alt = \""++a++"\"></img>"
transformBuilderBlock (Link a b) = "<a href = \"" ++ b ++ "\">" ++ a ++ "</a>"