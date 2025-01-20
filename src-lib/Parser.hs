{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}     -- cover all cases!
{-# OPTIONS_GHC -fwarn-unused-matches #-}          -- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}      -- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}          -- use different names!
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-} -- warn about incomplete patterns v2
{-# LANGUAGE InstanceSigs #-}
module Parser where
import GHC.Base (Alternative(..), Applicative(liftA2))

-- Взето от: https://github.com/fmi-fp-lab/fp-lab-2024-25/blob/master/exercises/09/Parser.hs
-- START

newtype Parser a = MkParser {runParser :: String -> [(String, a)]}

parse :: Parser a -> String -> Maybe a
parse px str =
  case runParser px str of
    [] -> Nothing
    (_, x) : _ -> Just x

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f px = MkParser $ \str ->
    [(str2, f res) | (str2, res)<-runParser px str]

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = MkParser $ \str -> [(str, x)]

  liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
  liftA2 f px py= MkParser $ \str -> [
        (rest2, f x y) |
        (rest1,x) <- runParser px str,
        (rest2, y) <- runParser py rest1]

instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) px f =
        MkParser $ \str ->
            [ (rest2, y)
            | (rest1, x) <- runParser px str
            , (rest2, y) <- runParser (f x) rest1
            ]

parseFailure :: Parser a
parseFailure = MkParser $ const []

instance Alternative Parser where
  empty :: Parser a
  empty = parseFailure

  (<|>) :: Parser a -> Parser a -> Parser a
  px <|> py =
    MkParser $ \str -> runParser px str ++ runParser py str

-- END