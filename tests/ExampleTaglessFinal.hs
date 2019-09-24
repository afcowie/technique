{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Data.Text
import Data.Void

type Parser = Parsec Void Text

class Expr a where
    add :: a -> a -> a
    sub :: a -> a -> a
    const :: Int -> a

instance Expr Int where
    add = (+)
    sub = (-)
    const = id

instance Expr String where
    add a b = "(" ++ a ++ "+" ++ b ++ ")"
    sub a b = "(" ++ a ++ "-" ++ b ++ ")"
    const = show

eval :: Int -> Int
eval = id

pretty :: String -> String
pretty = id

readExpr :: Expr a => Text -> Either String a
readExpr t = case parse (parseExpr <* eof) "<interactive>" t of
    Left b -> Left (errorBundlePretty b)
    Right a -> Right a

parseExpr :: Expr a => Parser a
parseExpr = choice
    [ try parseAdd
    , try parseSub
    , try parseSubExpr
    , parseConst
    ]

parseSubExpr :: Expr a => Parser a
parseSubExpr = choice
    [ parseConst
    , between (char '(') (char ')') parseExpr
    ]

parseConst :: Expr a => Parser a
parseConst = Main.const <$> decimal

parseSub :: Expr a => Parser a
parseSub = sub <$> parseSubExpr <* parseMinus <*> parseSubExpr
    where
        parseMinus = char '-'

parseAdd :: Expr a => Parser a
parseAdd = add <$> parseSubExpr <* parsePlus <*> parseSubExpr
    where
        parsePlus = char '+'

instance (Expr a, Expr b) => Expr (a,b) where
    add (a,a') (b,b') = ((add a b), (add a' b'))
    sub (a,a') (b,b') = ((sub a b), (sub a' b'))
    const i = (Main.const i, Main.const i)

prettyResult :: Text -> Either String String
prettyResult t = case readExpr t of
    Left e -> Left e
    Right (p, r) -> Right $ pretty p ++ " = " ++ show (eval r)
