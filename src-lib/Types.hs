{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
module Types where

data TextInformation = 
    Text String
    | Paragraph [TextInformation]
    | Bold TextInformation
    | Italic TextInformation
    | Header [TextInformation]
    | Header2 [TextInformation]
    | Link String String
    | Image String String
    | Blockquote [TextInformation]
    | NonOrderedList [TextInformation]
    | OrderedList [TextInformation]
    | Rule
    | Inline String
    deriving (Show, Eq)