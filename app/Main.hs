{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# OPTIONS_GHC -Werror #-}                        -- turn warnings into errors
module Main where
import Parser (parse)
import ParserMarkdown (parseLine)
import ComposeHtml (transformHtmlList)
import System.IO (openFile, IOMode(..), hGetContents)
import System.Environment (getArgs)


-- I like my text **bold** and *italic* `maybe code`

main :: IO ()
main = do
  res <- getArgs
  handle <- case res of 
            [] -> getLine >>= (`openFile` ReadMode) 
            (x: _) -> openFile x ReadMode
  linesLister <- hGetContents handle
  putStrLn $ transformHtmlList $ map (\str -> case parse parseLine str of
                                                Just x -> x
                                                Nothing -> error "Failed parsing") $ lines linesLister