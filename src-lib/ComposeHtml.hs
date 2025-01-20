{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant lambda" #-}
module ComposeHtml(transformHtmlList, transformHtml2) where

import Types (TextInformation(..))

generatedCss :: String
generatedCss = "<link href=\"https://fonts.googleapis.com/css2?family=Roboto:wght@400;700&display=swap\" rel=\"stylesheet\"> \
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
\            border-top: 1px solid #eee; \
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

transformHtmlList:: [TextInformation] -> String
transformHtmlList l = putInplaceHtml $ transformHtml2 l

transformHtml2 :: [TextInformation] -> String
transformHtml2 [] = ""
transformHtml2 l@((Blockquote _): _) = transformHtml (takeWhile func l) ++ transformHtml2 (dropWhile func l)
                                            where
                                                func :: TextInformation -> Bool
                                                func = \str -> case str of
                                                            (Blockquote _) -> True
                                                            _ -> False
transformHtml2 res@((NonOrderedList _): _) = transformHtml (takeWhile func res) ++ transformHtml2 (dropWhile func res)
                                            where
                                                func :: TextInformation -> Bool
                                                func = \str -> case str of
                                                            (NonOrderedList _) -> True
                                                            _ -> False
transformHtml2 res@((OrderedList _): _) = transformHtml (takeWhile func res) ++ transformHtml2 (dropWhile func res)
                                            where
                                                func :: TextInformation -> Bool
                                                func = \str -> case str of
                                                            (OrderedList _) -> True
                                                            _ -> False
transformHtml2 (x : xs) = transformHtml [x] ++ transformHtml2 xs

transformHtml:: [TextInformation] -> String
transformHtml [] = ""
transformHtml [Rule] = "\n<hr>\n"
transformHtml [Text l] = l
transformHtml [Italic t] = "<i>" ++ transformHtml [t] ++ "</i>"
transformHtml [Bold t] = "<b>" ++ transformHtml [t] ++ "</b>"
transformHtml [Inline t] = "<code>" ++ t ++ "</code>"
transformHtml [Paragraph p] = "<p>" ++ concatMap (transformHtml.(:[])) p ++ "</p>\n"
transformHtml [Header p] = "<h1>" ++ concatMap (transformHtml.(:[])) p ++ "</h1>\n"
transformHtml [Header2 p] = "<h2>" ++ concatMap (transformHtml.(:[])) p ++ "</h2>\n"
transformHtml [Header3 p] = "<h3>" ++ concatMap (transformHtml.(:[])) p ++ "</h3>\n"
transformHtml [Header4 p] = "<h4>" ++ concatMap (transformHtml.(:[])) p ++ "</h4>\n"
transformHtml [Header5 p] = "<h5>" ++ concatMap (transformHtml.(:[])) p ++ "</h5>\n"
transformHtml res@((Blockquote _): _) =
     "<blockquote>\n" ++ concatMap (
        \val -> case val of 
            (Blockquote l1) -> concatMap ((++" ").transformHtml.(:[])) l1
            _ -> error "Parsing text"
        ) res ++ "\n</blockquote>\n"
transformHtml res@((NonOrderedList _): _) =
     "<ul>\n" ++ concatMap (
        \val -> case val of 
                    (NonOrderedList l1) -> "<li>"++concatMap (transformHtml.(:[])) l1 ++ "</li>\n"
                    l ->  show l
        ) res ++ "</ul>\n"
transformHtml res@((OrderedList _): _) =
     "<ol>\n" ++ concatMap (
        \val -> case val of
                    (OrderedList l1) -> "<li>"++concatMap (transformHtml.(:[])) l1 ++ "</li>\n"
                    l -> show l
        ) res ++ "</ol>\n"

transformHtml _ = error "error parsing"